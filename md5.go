package main

import (
	"encoding/binary"
	"fmt"
	"math"
	"math/bits"
)

func md5(input string) {
	var s, K [64]uint32
	var i uint32

	s = [64]uint32{
		7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
		5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
		4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
		6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
	}

	for i = 0; i < 64; i++ {
		K[i] = uint32(math.Floor((1 << 32) * math.Abs(math.Sin(float64(i)+1))))
	}

	a0 := uint32(0x67452301)
	b0 := uint32(0xefcdab89)
	c0 := uint32(0x98badcfe)
	d0 := uint32(0x10325476)

	message := []byte(input)
	originalLen := len(message) * 8

	message = append(message, 0x80)
	for len(message)%64 != 56 {
		message = append(message, 0x0)
	}

	tmp := make([]byte, 4)
	binary.LittleEndian.PutUint32(tmp, uint32(originalLen))
	message = append(message, tmp...)

	for j := 0; j < len(message); j += 64 {
		chunk := message[j : j+64]

		M := make([]uint32, 16)
		for i = 0; i < 64; i += 4 {
			M[i/4] = binary.LittleEndian.Uint32(chunk[i : i+4])
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
			} else {
				F = C ^ (B | (^D))
				g = (7 * i) % 16
			}

			F += A + K[i] + M[g]
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

	digest := make([]byte, 0, 16)

	binary.LittleEndian.PutUint32(tmp, a0)
	digest = append(digest, tmp...)
	binary.LittleEndian.PutUint32(tmp, b0)
	digest = append(digest, tmp...)
	binary.LittleEndian.PutUint32(tmp, c0)
	digest = append(digest, tmp...)
	binary.LittleEndian.PutUint32(tmp, d0)
	digest = append(digest, tmp...)

	fmt.Printf("%x\n", digest)
}

func main() {
	md5("The quick brown fox jumps over the lazy dog")
	md5("The quick brown fox jumps over the lazy dog.")
	md5("")
}
