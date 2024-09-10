package main

import (
	"fmt"
	"time"
)

func main() {
	start := time.Now()
	s :=
		`fun main(): int { 
    		for x: y in iter {
				print(x, y, 10)
				do_err!??
				break
				continue
			} 
			return 0
			continue
			break aaaa
		}`
	fmt.Println("source bytes: ", len(s))
	parserManager := NewSourceManager(2)
	_ = parserManager.AddSource("some input", s)
	_ = parserManager.AddSource("some other input", s)
	roots := parserManager.ParseSources()
	printer := NewCSTPrinter(roots["some input"], s)
	printer.PrintRoot()
	x := NewCSTPrinter(roots["some other input"], s)
	x.PrintRoot()
	fmt.Println("time: ", time.Since(start).Microseconds(), "ms")
}
