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
	fmt.Printf("pos=%d char=%c\n", j.i, j.current())
}

func (j *JSON) back() byte {
	return j.raw[j.i-1]
}

func (j *JSON) peek() byte {
	return j.raw[j.i+1]
}

func (j *JSON) next() byte {
	j.i++
	return j.current()
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
		c := j.current()

		if j.isWhitespace() || c == ',' || c == '}' || c == ']' {
			break
		} else if j.isDigit() {
			s += string(c)
		} else {
			panic("Unexpected character near int " + string(c))
		}

		j.next()
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func (j *JSON) parseArray() []any {
	arr := []any{}

	if j.current() != '[' {
		panic("Expected opening '['")
	}
	j.next()

	expectComma := false

	for j.iter() {
		c := j.current()

		j.print()

		if j.isWhitespace() {
			j.skipWhitespace()
		} else if c == ']' {
			j.next()
			return arr
		} else if expectComma && c != ',' {
			panic("Expected comma")
		} else if c == ',' {
			j.next()
		} else if j.isDigit() {
			arr = append(arr, j.parseInt())
			expectComma = true
		} else if c == '"' {
			arr = append(arr, j.parseString())
			expectComma = true
		} else {
			panic("Unexpected array character " + string(c))
		}
	}

	panic("Expected closing ']'")
}

func (j *JSON) Parse() map[any]any {
	var m map[any]any

	isKey := true
	var key any

	// colonExpectedAndNotFound := false

	for j.iter() {
		fmt.Println(m)

		if j.isWhitespace() {
			j.skipWhitespace()
			continue
		}

		switch c := j.current(); c {
		case '{':
			m = make(map[any]any)
			j.next()
		case '"':
			if isKey {
				key = j.parseString()
			} else {
				m[key] = j.parseString()
			}
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if isKey {
				panic("int can't be key")
			}
			m[key] = j.parseInt()
		case ':':
			isKey = false
			j.next()
		case ',':
			isKey = true
			j.next()
		case '}':
			return m
		case '[':
			if isKey {
				panic("array can't be key")
			}
			m[key] = j.parseArray()
		default:
			panic("Unexpected character " + string(c))
		}
	}

	panic("Expected closing '}'")
}

func main() {
	json := `{"abc": 123, "test": "def", "array": [1, 2, 3]}`
	fmt.Println(NewJSON(json).Parse())
}
