package main

import (
	"fmt"
	"time"
)

func main() {
	start := time.Now()
	s := "class Point { x: int, y: int }"
	fmt.Println(len(s))
	parser := NewParser(s)
	fmt.Println(parser.nextBlock())
	fmt.Println(time.Since(start).Microseconds())
}
