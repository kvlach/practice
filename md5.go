package main

import (
	"encoding/binary"
	"fmt"
	"math"
)

var s = [64]uint32{
	7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
	5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
	4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
	6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
}
var K = [64]uint32{}
var i uint32 = 0

func main() {
	for i := 0; i < 64; i++ {
		K[i] = uint32(math.Floor((1 << 32) * math.Abs(math.Sin(float64(i)+1))))
	}

	fmt.Println(K)

	a0 := 0x67452301
	b0 := 0xefcdab89
	c0 := 0x98badcfe
	d0 := 0x10325476

	message := []byte("this is the message")
	originalLen := len(message)
	message = append(message, '1')

	for len(message) != 448%512 {
		message = append(message, '0')
	}

	bs := make([]byte, 4)
	binary.LittleEndian.AppendUint32(bs, uint32(originalLen))
	message = append(message, bs...)

	for j := 0; j < len(message); j += 512 {
		chunk := message[j : j+512]

		words := make([]byte, 16)
		for i := 0; i < len(chunk); i += 32 {
			words = append(words, chunk[i:i+32]...)
		}

		A := a0
		B := b0
		C := c0
		D := d0

		for i := 0; i < 64; i++ {
			var F, g int

			if 0 <= i && i <= 15 {
				F = (B & C) | ((^B) & D)
				g = i
			} else if 16 <= i && i <= 31 {
				F = (D & B) | ((^D) & C)
				g = (5*i + 1) % 16
			} else if 32 <= i && i <= 47 {
				F = B ^ C ^ D
				g = (3*i + 5) % 16
			} else if 48 <= i && i <= 63 {
				F = C ^ (B | (^D))
				g = (7 * i) % 16
			}

			F = F + A + K[i] + M[g]
			A = D
			D = C
			C = B
			B = B + leftRotate(F, s[i])
		}
	}

}
