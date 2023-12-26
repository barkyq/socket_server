package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"github.com/fsnotify/fsnotify"
)

var root_file = flag.String("f", "", "file of tracked git repositories")

func main() {
	flag.Parse()

	file_list, err := func(root_file string) ([]string, error) {
		file_list := make([]string, 0)
		if f, e := os.Open(root_file); e != nil {
			return nil, fmt.Errorf("please set -f flag to a file with git repositories\n")
		} else {
			b := bufio.NewReader(f)
			for {
				if line, e := b.ReadSlice('\n'); e != nil && e != io.EOF {
					return nil, e
				} else if e == io.EOF {
					return file_list, nil
				} else {
					file_list = append(file_list, filepath.Clean(strings.TrimSpace(fmt.Sprintf("%s", line))))
					continue
				}
			}
		}
	}(*root_file)
	if err != nil {
		panic(err)
	}

	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		panic(err)
	}

	uchan := make(chan string)
	for k, f := range file_list {
		if hf, e := os.Open(filepath.Join(f, ".git", "HEAD")); e != nil {
			fmt.Fprintf(os.Stderr, "BAD: %s\n", f)
		} else if e := watcher.Add(filepath.Join(f, ".git")); e != nil {
			fmt.Fprintf(os.Stderr, "BAD: %s\n", f)
		} else if b, e := io.ReadAll(hf); e == nil {
			hf.Close()
			go func(f string, body []byte) {
				if content, e := parse_head_file(f, body); e == nil {
					uchan <- content
				}
			}(f, b)
			continue
		} else {
			panic(e)
		}
		if k < len(file_list)-1 {
			tmp := file_list[k+1:]
			file_list = file_list[:k]
			file_list = append(file_list, tmp...)
		} else {
			file_list = file_list[:k]
		}
	}

	if rf, e := os.OpenFile(*root_file, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644); e == nil {
		for _, f := range file_list {
			rf.Write([]byte(f))
			rf.Write([]byte{'\n'})
		}
		rf.Close()
	} else {
		panic(e)
	}

	nfchan := make(chan string)
	go func() {
		b := make([]byte, 0)
	outer:
		for {
			select {
			case f := <-nfchan:
				f = filepath.Clean(strings.TrimSpace(f))
				for _, ef := range file_list {
					if ef == f {
						continue outer
					}
				}
				file_list = append(file_list, f)
				if hf, e := os.Open(filepath.Join(f, ".git", "HEAD")); e != nil {
					fmt.Println(e)
					hf.Close()
				} else if b, e := io.ReadAll(hf); e == nil {
					hf.Close()
					go func(f string, body []byte) {
						if content, e := parse_head_file(f, body); e == nil {
							uchan <- content
						}
					}(f, b)
					if e := watcher.Add(filepath.Join(f, ".git")); e != nil {
						fmt.Println(e)
						hf.Close()
					}
					if rf, e := os.OpenFile(*root_file, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644); e == nil {
						rf.Write([]byte(f))
						rf.Write([]byte{'\n'})
						rf.Close()
					}
				}
			case ev, ok := <-watcher.Events:
				if !ok {
					panic(ok)
				}
				if ev.Has(fsnotify.Create) && filepath.Base(ev.Name) == "HEAD" {
					if f, e := os.Open(ev.Name); e != nil {
						panic(e)
					} else if b, e = io.ReadAll(f); e == nil {
						f.Close()
						if content, e := parse_head_file(filepath.Dir(filepath.Dir(ev.Name)), b); e == nil {
							uchan <- content
						}
					} else {
						panic(e)
					}
				}
			case _, ok := <-watcher.Errors:
				if !ok {
					panic(ok)
				}
			}
		}
	}()

	wbs := make([]*bufio.Writer, 0)
	status := make([]string, 0)
	var mu sync.Mutex

	// msg handler
	go func() {
		for msg := range uchan {
			fmt.Println(msg)
			bmsg := []byte(msg)
			mu.Lock()
			for _, wb := range wbs {
				if k, e := wb.Write(bmsg); e != nil {
					if k+1 < len(wbs) {
						tmp := wbs[k+1:]
						wbs = wbs[:k]
						wbs = append(wbs, tmp...)
					} else {
						wbs = wbs[:k]
					}
					continue
				} else {
					wb.Write([]byte{'\n'})
					wb.Flush()
				}
			}
			for k, s := range status {
				if strings.Split(s, " ")[0] == strings.Split(msg, " ")[0] {
					status[k] = msg
					goto jump
				}
			}
			status = append(status, msg)
		jump:
			mu.Unlock()
		}
	}()

	sock_addr := fmt.Sprintf("%s.socket", *root_file)
	if e := os.RemoveAll(sock_addr); e != nil {
		panic(e)
	}
	l, e := net.Listen("unix", sock_addr)
	if e != nil {
		panic(e)
	}
	defer l.Close()

	for {
		if c, e := l.Accept(); e != nil {
			c.Close()
			continue
		} else {
			wb := bufio.NewWriter(c)
			rb := bufio.NewReader(c)
			go func() {
				for {
					if b, e := rb.ReadSlice('\n'); e != nil {
						return
					} else {
						nfchan <- fmt.Sprintf("%s", b)
					}
				}
			}()
			mu.Lock()
			for _, state := range status {
				wb.Write([]byte(state))
				wb.Write([]byte{'\n'})
			}
			wb.Flush()
			wbs = append(wbs, wb)
			mu.Unlock()
		}
	}
}

func parse_head_file(filename string, body []byte) (string, error) {
	content := strings.TrimSpace(fmt.Sprintf("%s", body))
	if strings.HasPrefix(content, "ref: refs/heads/") {
		content = strings.TrimPrefix(content, "ref: refs/heads/")
	} else {
		content = "D:" + content[0:6]
	}
	return fmt.Sprintf("%s/ %s:%s", filename, filepath.Base(filename), content), nil
}
