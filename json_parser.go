package main

import (
	"fmt"
	"strconv"
)

func parse(input string) {
	i := 0
	var c byte

	peek := func() byte {
		i += 1
		c = input[i]
		return c
	}

	skipWhitespace := func() {
		for {
			switch peek() {
			case ' ', '\t', '\n':
				continue
			default:
				return
			}
		}
	}

	parseInt := func() int {
		s := string(c)
		for {
			switch peek(); c {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				s += string(c)
			default:
				i, err := strconv.Atoi(s)
				if err != nil {
					panic(err)
				}
				return i
			}
		}
	}

	for ; i < len(input); i++ {
		c = input[i]

		switch c {
		case '{':
			switch peek() {
			case '"':
				var key string
				for {
					c = peek()
					if c == '"' {
						break
					}
					key += string(c)
				}

				fmt.Println("key =", key)

				if peek() != ':' {
					panic(fmt.Errorf("Expected ':' at %d", i))
				}

				skipWhitespace()

				switch c {
				case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
					i := parseInt()
					fmt.Println("val =", i)
				}

				switch peek() {
				case ',':
					// parse from top of
				case '}':
					continue
				default:
					panic(fmt.Errorf("Unexpected closing token '%c'", c))
				}
			}
		default:
			panic(fmt.Errorf("Unexpected token '%c'", c))
		}
	}
}

func main() {
	json := `{"abc": 123}`
	parse(json)
}
