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
	fmt.Printf("pos=%d, char=%c\n", json.i, json.current())
}

func (json *JSON) back() byte {
	return json.raw[json.i-1]
}

func (json *JSON) next() byte {
	json.i++
	return json.current()
}

func (json *JSON) isDigit() bool {
	switch json.current() {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return true
	default:
		return false
	}
}

func (json *JSON) isWhitespace() bool {
	switch json.current() {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func (json *JSON) skipWhitespace() {
	for json.i < len(json.raw) {
		if json.isWhitespace() {
			json.next()
		} else {
			break
		}
	}
}

func (json *JSON) parseInt() int {
	var s string

	for json.i < len(json.raw) {
		if json.isDigit() {
			s += string(json.current())
		} else if json.current() == ',' || json.current() == '}' {
			break
		} else if json.isWhitespace() {
			break
		}
		json.next()
	}

	i, err := strconv.Atoi(s)
	if err != nil {
		panic("Invalid int")
	}
	return i
}

func (json *JSON) parseString() string {
	var s string

	if json.current() != '"' {
		panic(`Expected opening '"'`)
	}

	for json.i < len(json.raw)-1 {
		c := json.next()
		if c == '"' && json.back() != '\\' {
			break
		}
		s += string(c)
	}

	return s
}

func (json *JSON) Parse() {

	var m map[string]any

	isKey := true
	var key string
	var val any

	for json.i < len(json.raw) {
		// json.print()

		if json.isWhitespace() {
			json.skipWhitespace()
			continue
		}

		if json.isDigit() {
			val = json.parseInt()
			m[key] = val
			continue
		}

		if json.current() == '"' {
			str := json.parseString()

			if isKey {
				key = str
			} else {
				m[key] = str
			}

			json.next()
			continue
		}

		if json.current() == ':' {
			json.next()
			json.skipWhitespace()
			isKey = false
			continue
		}

		if json.current() == '{' {
			m = make(map[string]any)
			json.next()
			continue
		}

		if json.current() == ',' {
			isKey = true
			json.next()
			continue
		}

		if json.current() == '}' {
			break
		}

		// json.next()
	}

	fmt.Println(m)
}

func main() {
	json := `{"ab\"c": 123, "second": "test"}`
	NewJSON(json).Parse()
}
