package main

import (
	"fmt"
	"strconv"
)

type JSON struct {
	i   int
	raw string
}

func NewJSON(json string) *JSON {
	return &JSON{
		i:   0,
		raw: json,
	}
}

func (json *JSON) current() byte {
	return json.raw[json.i]
}

func (json *JSON) print() {
	fmt.Printf("%c\n", json.current())
}

func (json *JSON) back() byte {
	return json.raw[json.i-1]
}

func (json *JSON) next() byte {
	json.i++
	return json.raw[json.i]
}

func (json *JSON) skipWhitespace() {
	for json.i < len(json.raw) {
		switch json.current() {
		case ' ', '\t', '\n', '\r':
			json.next()
		default:
			return
		}
	}
}

func (json *JSON) parseString() string {
	if json.current() != '"' {
		panic(`Expected '"'`)
	}

	var s string

	for json.i < len(json.raw) {
		c := json.next()
		if c == '"' && json.back() != '\\' {
			return s
		}
		s += string(c)
	}

	panic("String never closes")
}

func (json *JSON) parseInt() int {
	var s string

	for json.i < len(json.raw) {
		switch json.current() {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			s += string(json.current())
		case ' ', '\t', '\n', '\r':
			json.skipWhitespace()
		case ',', '}':
			i, err := strconv.Atoi(s)
			if err != nil {
				panic("Invalid int")
			}
			return i
		default:
			panic(fmt.Errorf("Unpexpected character '%c'", json.current()))
		}
		json.next()
	}

	panic("unreachable")
}

func (json *JSON) parseObject() {
	if json.current() != '{' {
		panic("Expected opening '{'")
	}

	for json.i < len(json.raw) {
		switch c := json.next(); c {
		case '"':
			fmt.Println("string =", json.parseString())
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			fmt.Println("int =", json.parseInt())
		case ' ', '\t', '\n', '\r':
			json.skipWhitespace()
		case ':':
			// json.next()
			// json.skipWhitespace()
		case ',':
			// json.next()
			// json.skipWhitespace()
		case '}':
			return
		}
	}
}

func (json *JSON) Parse() {
	json.parseObject()
}

func main() {
	json := `{"abc": 123, "def": "xyz"}`
	NewJSON(json).Parse()
}
