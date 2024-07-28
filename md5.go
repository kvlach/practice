package main

import (
	"encoding/binary"
	"fmt"
	"math"
	"math/bits"
)

func main() {
	var K [64]uint32
	var i uint32

	s := [64]uint32{
		7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
		5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
		4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
		6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
	}

	for i := 0; i < 64; i++ {
		K[i] = uint32(math.Floor((1 << 32) * math.Abs(math.Sin(float64(i)+1))))
	}

	a0 := uint32(0x67452301)
	b0 := uint32(0xefcdab89)
	c0 := uint32(0x98badcfe)
	d0 := uint32(0x10325476)

	message := []byte("this is a message")
	var originalLen uint64 = uint64(len(message))

	message = append(message, 0x80)
	for len(message)%64 != 56 {
		message = append(message, 0x0)
	}

	fmt.Println(len(message), message)

	bs := make([]byte, 4)
	binary.LittleEndian.AppendUint32(bs, uint32(originalLen))

	for j := 0; j < len(message); j += 64 {
		chunk := message[j : j+64]

		var M [16]uint32
		for i := 0; i < len(chunk); i += 4 {
			binary.LittleEndian.PutUint32(chunk[i:i+4], M[i])
		}

		A := a0
		B := b0
		C := c0
		D := d0

		for i = 0; i < 64; i++ {
			var F, g uint32

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
			B += bits.RotateLeft32(F, int(s[i]))
		}

		a0 += A
		b0 += B
		c0 += C
		d0 += D
	}

	var digest []byte
	binary.LittleEndian.AppendUint32(digest, a0)
	binary.LittleEndian.AppendUint32(digest, b0)
	binary.LittleEndian.AppendUint32(digest, c0)
	binary.LittleEndian.AppendUint32(digest, d0)

	fmt.Println(string(digest))
}
