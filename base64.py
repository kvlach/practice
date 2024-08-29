alphabet = [
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
	'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
	'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
	'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/',
]

def encode(string):
	bs = string.encode('utf-8')

	for i in range(0, len(bs)-len(bs)%3, 3):
		b1 = bs[i]
		b2 = bs[i+1]
		b3 = bs[i+2]

		c1 = alphabet[b1>>2]
		c2 = alphabet[((b1&0b00000011) << 4) | b2>>4]
		c3 = alphabet[((b2&0b00001111) << 2) | b3>>6]
		c4 = alphabet[b3&0b00111111]

		yield c1 + c2 + c3 + c4

	if len(bs)%3 == 1:
		b1 = bs[-1]
		c1 = alphabet[b1>>2]
		c2 = alphabet[(b1&0b00000011) << 4]
		yield c1 + c2  + '=='

	elif len(bs)%3 == 2:
		b1 = bs[-2]
		b2 = bs[-1]
		c1 = alphabet[b1>>2]
		c2 = alphabet[((b1&0b00000011) << 4) | b2>>4]
		c3 = alphabet[(b2&0b00001111) << 2]
		yield c1 + c2 + c3 + '='



if __name__ == '__main__':
	string = 'light work'
	print(string, ''.join(encode(string)))
