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

var root_directory = flag.String("rd", "", "directory to find git repositories")

func main() {
	flag.Parse()
	if f, e := os.Stat(*root_directory); e != nil || !f.IsDir() {
		fmt.Fprintf(os.Stderr, "please set -rd flag to a valid directory\n")
	} else {
		fmt.Fprintf(os.Stderr, "watching %s", f.Name())
	}

	sock_addr := filepath.Join(*root_directory, ".socket")
	if e := os.RemoveAll(sock_addr); e != nil {
		panic(e)
	}
	l, e := net.Listen("unix", sock_addr)
	if e != nil {
		panic(e)
	}
	defer l.Close()

	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		e = err
		return
	}

	uchan := make(chan string)
	if dirs, e := os.ReadDir(*root_directory); e != nil {
		panic(e)
	} else {
		for _, dir := range dirs {
			if !dir.IsDir() {
				continue
			}
			git_dir := filepath.Join(*root_directory, dir.Name(), ".git")
			if _, e := os.Stat(git_dir); e != nil {
				continue
			} else if e = watcher.Add(git_dir); e != nil {
				panic(e)
			} else {
				if f, e := os.Open(filepath.Join(git_dir, "HEAD")); e != nil {
					panic(e)
				} else if b, e := io.ReadAll(f); e == nil {
					f.Close()
					go func() {
						content := strings.TrimSpace(fmt.Sprintf("%s", b))
						if strings.HasPrefix(content, "ref: refs/heads/") {
							content = strings.TrimPrefix(content, "ref: refs/heads/")
						} else {
							content = "detached:" + content[0:6]
						}
						uchan <- fmt.Sprintf("%s/ %s", filepath.Dir(git_dir), content)
					}()
				} else {
					panic(e)
				}

			}
		}
	}

	go func() {
		b := make([]byte, 0)
		var content string
		for {
			select {
			case ev, ok := <-watcher.Events:
				if !ok {
					panic(ok)
				}
				if ev.Has(fsnotify.Create) && filepath.Base(ev.Name) == "HEAD" {
					if f, e := os.Open(ev.Name); e != nil {
						panic(e)
					} else if b, e = io.ReadAll(f); e == nil {
						f.Close()
						content = strings.TrimSpace(fmt.Sprintf("%s", b))
						if strings.HasPrefix(content, "ref: refs/heads/") {
							content = strings.TrimPrefix(content, "ref: refs/heads/")
						} else {
							content = "detached:" + content[0:6]
						}
						uchan <- fmt.Sprintf("%s/ %s", filepath.Dir(filepath.Dir(ev.Name)), content)
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
	go func() {
		for msg := range uchan {
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
	for {
		if c, e := l.Accept(); e != nil {
			panic(e)
		} else {
			wb := bufio.NewWriter(c)
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
