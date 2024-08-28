alphabet = [
	'A',
	'B',
	'C',
	'D',
	'E',
	'F',
	'G',
	'H',
	'I',
	'J',
	'K',
	'L',
	'M',
	'N',
	'O',
	'P',
	'Q',
	'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    '+',
    '/',
]

# wikipedia example
string = "Many hands make light work."

bs = string.encode('utf-8')
b64 = []

def b64fy(b1, b2, b3):
	# if b2 is None:
	# 	b2 = 0
	# if b3 is None:
	# 	b3 = 0

	padding = '='.encode('utf-8')[0]

	b64.append(alphabet[b1 >> 2])
	b64.append(alphabet[(b1&0b00000011)<<4 | (b2&0b11110000)>>4])
	b64.append(alphabet[(b2&0b00001111)<<2 | b3>>6])
	# b64.append(alphabet[b3&0b00111111])

	if b2 is None and b3 is None:
		b64.append(padding)
		b64.append(padding)
	elif b3 is None:
		b64.append(padding)


for i in range(0, len(string)-len(string)%3, 3):
	b1 = bs[i]
	b2 = bs[i+1]
	b3 = bs[i+2]
	b64fy(b1, b2, b3)

if len(string) % 3 == 1:
	b64fy(bs[-1], None, None)
elif len(string) % 3 == 2:
	b64fy(bs[-1], bs[-2], None)

print(''.join(b64))

