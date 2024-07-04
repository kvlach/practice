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

func (json *JSON) back() byte {
	return json.raw[json.i-1]
}

func (json *JSON) peek() byte {
	return json.raw[json.i+1]
}

func (json *JSON) next() byte {
	json.i++
	return json.current()
}

func (json *JSON) iter() bool {
	return json.i < len(json.raw)
}

func (json *JSON) print() {
	fmt.Printf("pos=%d, char=%c\n", json.i, json.current())
}

func (json *JSON) isWhitespace() bool {
	switch json.current() {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func (json *JSON) isDigit() bool {
	switch json.current() {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return true
	default:
		return false
	}
}

func (json *JSON) skipWhitespace() {
	for json.iter() {
		if json.isWhitespace() {
			json.next()
		}
		break
	}
}

func (json *JSON) parseString() string {
	if json.current() != '"' {
		panic(`Expected opening '"'`)
	}

	var s string

	for json.iter() {
		c := json.next()
		if c == '"' && json.back() != '\\' {
			json.next()
			break
		}
		s += string(c)
	}

	return s
}

func (json *JSON) parseInt() int {
	var s string

	for json.iter() {
		c := json.current()

		if json.isDigit() {
			s += string(c)
		} else if json.isWhitespace() || c == ',' || c == '}' || c == ']' {
			break
		} else {
			panic("Unexpected int character " + string(c))
		}

		json.next()
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func (json *JSON) parseArray() []any {
	if json.current() != '[' {
		panic("Expected opening [")
	}
	json.next()

	arr := []any{}

	for json.iter() {
		switch c := json.current(); c {
		case '"':
			arr = append(arr, json.parseString())
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			arr = append(arr, json.parseInt())
		case ',':
			json.next()
		case ' ', '\t', '\n', '\r':
			json.skipWhitespace()
		case ']':
			json.next()
			return arr
		default:
			panic("Unexpected character in array " + string(c))
		}
	}

	panic("unreachable")
}

func (json *JSON) Parse() map[string]any {
	var m map[string]any

	isKey := true
	var key string

	for json.iter() {
		c := json.current()

		switch c {
		case '{':
			m = make(map[string]any)
			json.next()
		case '"':
			if isKey {
				key = json.parseString()
			} else {
				m[key] = json.parseString()
				isKey = true
			}
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if isKey {
				panic("int can't be key")
			}
			m[key] = json.parseInt()
			isKey = true
		case '[':
			if isKey {
				panic("array can't be key")
			}
			m[key] = json.parseArray()
		case ' ', '\t', '\n', '\r':
			json.skipWhitespace()
		case ':':
			isKey = false
			json.next()
		case ',':
			isKey = true
			json.next()
		case '}':
			return m
		default:
			panic("Unexpected character " + string(c) + ".")
		}
	}

	panic("unreachable")
}

func main() {
	json := `{"abc": 123, "test": "def", "third": 56, "array": [1, 2, 3]}`
	parsed := NewJSON(json).Parse()
	fmt.Println(parsed)
}
