package main

import (
	"fmt"
	"strconv"
)

var i int = 0

type JSON struct {
	i   int
	raw string
}

func NewJSON(input string) *JSON {
	return &JSON{
		i:   0,
		raw: input,
	}
}

func (json *JSON) current() byte {
	return json.raw[json.i]
}

func (json *JSON) next() byte {
	json.i++
	return json.current()
}

func (json *JSON) peek() byte {
	return json.raw[json.i+1]
}

func (json *JSON) back() byte {
	return json.raw[json.i-1]
}

func (json *JSON) skipWhitespace() {

	for {
		switch json.next() {
		case ' ', '\t', '\n', '\r':
			continue
		default:
			return
		}
	}
}

func (json *JSON) parseString() string {
	var s string
	for json.i < len(json.raw) {
		c := json.next()
		if c == '"' && json.back() != '\\' {
			return s
		}
		s += string(c)
	}
	panic(`Expected closing '"'`)
}

func (json *JSON) parseInt() int {
	var s string

	for json.i < len(json.raw) {

		switch c := json.current(); c {
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			s += string(c)
		case ',', '}':
			break
		default:
			panic("Expected either ',' or '}'")
		}
		json.next()
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		panic(fmt.Errorf("Invalid int '%s'", s))
	}
	return i
}

func (json *JSON) parseValue() any {
	json.next()
	json.skipWhitespace()
	if json.current() != ':' {
		panic("Expected ':'")
	}
	json.skipWhitespace()

	switch json.current() {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return json.parseInt()
	}

	return nil
}

func (json *JSON) parseObject() {
	if json.current() != '{' {
		panic("Expected opening '{'")
	}

	var key string
	var val any

	for {
		switch c := json.next(); c {
		case '"':
			key = json.parseString()
			val = json.parseValue()
		default:
			panic("TODO")
		}

		fmt.Println(key)
		fmt.Println(val)
	}
}

func parse(input string) {
	json := NewJSON(input)
	json.parseObject()

	// for i < len(json) {
	// 	//
	// }
}

func main() {
	json := `{"ab\"c"   : 123}`
	parse(json)
}
