def merge(left, right):
	result = []

	while left and right:
		if left[0] <= right[0]:
			result.append(left[0])
			left.pop(0)
		else:
			result.append(right[0])
			right.pop(0)

	if left:
		result += left
	elif right:
		result += right

	return result

def merge_sort(lst):
	if len(lst) <= 1:
		return lst

	left = lst[:len(lst)//2]
	right = lst[len(lst)//2:]

	left = merge_sort(left)
	right = merge_sort(right)

	return merge(left, right)

# [3], [5], [2], [1], [6], [3], [4]
#  \    /    \    /    \    /    |
#  [3, 5]    [1, 2]    [3, 6]   [4]
#     \         /         \      /
#    [1, 2, 3, 5]        [3, 4, 6]
#          \                 /
#         [1, 2, 3, 3, 4, 5, 6]

if __name__ == '__main__':
	lst = [3, 5, 2, 1, 6, 3, 4]
	print(merge_sort(lst))
