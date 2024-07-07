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

func (j *JSON) next() byte {
	j.i++
	return j.current()
}

func (j *JSON) back() byte {
	return j.raw[j.i-1]
}

func (j *JSON) peek() byte {
	return j.raw[j.i+1]
}

func (j *JSON) print() {
	fmt.Printf("pos=%d char=%c\n", j.i, j.current())
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
		panic(`Expected opening "`)
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

	panic(`Expected closing "`)
}

func (j *JSON) parseInt() int {
	var s string

	for j.iter() {
		c := j.current()
		if j.isDigit() {
			s += string(c)
			j.next()
		} else if j.isWhitespace() || c == ',' || c == '}' || c == ']' {
			break
		} else {
			panic("unexpected character for int " + string(c))
		}
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func (j *JSON) parseArray() []any {
	if j.current() != '[' {
		panic("Expected opening [")
	}
	j.next()

	arr := []any{}

	// expectComma := false

	for j.iter() {
		c := j.current()

		switch c {
		case '"':
			arr = append(arr, j.parseString())
			// expectComma = true
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			arr = append(arr, j.parseInt())
			// expectComma = true
		case '{':
			arr = append(arr, j.parseObject())
			// expectComma = true
		case ' ', '\t', '\n', '\r':
			j.skipWhitespace()
		case ',':
			// expectComma = false
			j.next()
		case ']':
			if j.i != len(j.raw)-1 {
				j.next()
			}
			return arr
		default:
			panic("unexpected array char " + string(c))
		}
	}

	panic("Expected closing ]")
}

func (j *JSON) parseObject() map[any]any {
	if j.current() != '{' {
		panic("Expected opening {")
	}
	j.next()

	m := make(map[any]any)
	expectKey := true
	var key any

	for j.iter() {
		c := j.current()

		switch c {
		case '"':
			if expectKey {
				key = j.parseString()
			} else {
				m[key] = j.parseString()
			}
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if expectKey {
				panic("ints can't be keys")
			}
			m[key] = j.parseInt()
		case '[':
			if expectKey {
				panic("arrays can't be keys")
			}
			m[key] = j.parseArray()
		case ' ', '\t', '\n', '\r':
			j.skipWhitespace()
		case ':':
			expectKey = false
			j.next()
		case ',':
			expectKey = true
			j.next()
		case '}':
			if j.i != len(j.raw)-1 {
				j.next()
			}
			return m
		default:
			panic("unexpected object character " + string(c))
		}
	}

	panic("Expected closing }")
}

func (j *JSON) Parse() any {
	for j.iter() {
		switch c := j.current(); c {
		case '[':
			return j.parseArray()
		case '{':
			return j.parseObject()
		default:
			panic("unexpected character " + string(c))
		}
	}
	panic("unreachable")
}

func main() {
	json := `[{"abc": "def", "integer": 123, "nested_array_in_object": [{"first_key": "first_value"}, {"second_key": "second_value"}]}]`
	// json := `{"abc": 123}`
	fmt.Println(NewJSON(json).Parse())
}
