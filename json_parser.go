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

func (j *JSON) current() byte {
	return j.raw[j.i]
}

func (j *JSON) print() {
	fmt.Printf("pos=%d, char=%c\n", j.i, j.current())
}

func (j *JSON) next() byte {
	j.i++
	return j.current()
}

func (j *JSON) peek() byte {
	return j.raw[j.i+1]
}

func (j *JSON) back() byte {
	return j.raw[j.i-1]
}

func (j *JSON) iter() bool {
	return j.i < len(j.raw)
}

func (j *JSON) isDigit() bool {
	switch j.current() {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return true
	default:
		return false
	}
}

func (j *JSON) isWhitespace() bool {
	switch j.current() {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func (j *JSON) skipWhitespace() {
	for j.iter() {
		if !j.isWhitespace() {
			return
		}
		j.next()
	}
}

func (j *JSON) parseString() string {
	if j.current() != '"' {
		panic(`Expected opening '"'`)
	}

	var s string

	for j.iter() {
		c := j.next()
		if c == '"' && j.back() != '\\' {
			j.next()
			return s
		}
		s += string(c)
	}

	panic(`Expected closing '"'`)
}

func (j *JSON) parseInt() int {
	var s string

	for j.iter() {
		if !j.isDigit() {
			if j.isWhitespace() {
				break
			}
			c := j.current()
			if c == ',' || c == ']' || c == '}' {
				break
			}
			panic("Unexpected int ending character " + string(j.current()))
		}
		s += string(j.current())
		j.next()
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func (j *JSON) parseArray() []any {
	if j.current() != '[' {
		panic(`Expected opening '['`)
	}
	j.next()
	arr := []any{}

	expectComma := false

	for j.iter() {
		if j.isWhitespace() {
			j.skipWhitespace()
		} else if expectComma && j.current() != ',' && j.peek() != ']' {
			j.print()
			panic("Expected comma in array")
		} else if expectComma && j.current() == ',' {
			j.next()
		} else if j.isDigit() {
			arr = append(arr, j.parseInt())
			expectComma = true
		} else if j.current() == '"' {
			arr = append(arr, j.parseString())
			expectComma = true
		} else if j.current() == ']' {
			return arr
		} else {
			panic("Unexpected character in array " + string(j.current()))
		}
	}

	panic("Expected closing ] in array")
}

func (j *JSON) Parse() map[any]any {
	var m map[any]any

	isKey := true
	var key any

	// inObj := true
	var obj map[any]any

	for j.iter() {
		fmt.Println(obj)
		// j.print()
		switch c := j.current(); c {
		case '{':
			// inObj = true
			obj = make(map[any]any)
			j.next()
		case '}':
			return m
		case ':':
			isKey = false
			j.next()
		case ',':
			isKey = true
			j.next()
		case '"':
			if isKey {
				key = j.parseString()
			} else {
				obj[key] = j.parseString()
			}
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if isKey {
				panic("ints can't be keys")
			}
			obj[key] = j.parseInt()
		case '[':
			if isKey {
				panic("array can't be key")
			}
			obj[key] = j.parseArray()
		case ' ', '\t', '\n', '\r':

			j.print()
			j.skipWhitespace()
			j.print()
		default:
			panic("Unexpected obj character " + string(j.current()))
		}
	}

	return m
}

func main() {
	json := `{"abc": 123, "test": "def", "arrayy": [1, 3, 6]}`
	fmt.Println(NewJSON(json).Parse())
}
