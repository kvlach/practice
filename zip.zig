const std = @import("std");

const LeafNode = struct {
	symbol: u16,
	probability: f64,
	parent: ?*LeafNode,

	fn init(symbol: u16, probability: f64) LeafNode {
		return LeafNode{
			.symbol = symbol,
			.probability = probability,
			.parent = null,
		};
	}
};

const PriorityQueue = struct {
	queue: std.ArrayList(LeafNode),
	allocator: std.mem.Allocator,

	fn init(allocator: std.mem.Allocator) !PriorityQueue {
		return PriorityQueue{
			.queue = std.ArrayList(LeafNode).init(allocator),
			.allocator = allocator,
		};
	}

	fn lowest_probability_index(self: *PriorityQueue) usize {
		var lowest_index: usize = 0;
		var lowest_prob: f64 = 1000000;
		for (0.., self.queue.items) |i, node| {
			std.log.debug("iterating queue, {} {}", .{lowest_prob, node.probability});
			if (lowest_prob > node.probability) {
				lowest_index = i;
				lowest_prob = node.probability;
			}
		}
		return lowest_index;
	}

	fn append(self: *PriorityQueue, node: LeafNode) !void {
		try self.queue.append(node);
	}

	fn remove(self: *PriorityQueue, index: usize) {
		self.queue.swapRemove(index);
	}

	fn pop_lowest_probability(self: *PriorityQueue) LeafNode {
		const node_index = self.lowest_probability_index();
		const node = self.queue.items[node_index];
		self.remove(node_index);
		return node;
	}
};

// fn highest_probability(priority_queue: std.ArrayList(LeafNode)) usize {
// 	var highest_index: usize = 0;
// 	var highest_prob: f64 = 0;
// 	for (0.., priority_queue.items) |i, node| {
// 		if (node.probability > highest_probability) {
// 			highest_index = i;
// 			highest_probability = node.probability;
// 		}
// 	}
// 	return highest_index;
// }

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();
	var priority_queue = try PriorityQueue.init(allocator);

	const node1 = LeafNode.init(1, 0.6);
	const node2 = LeafNode.init(2, 0.3);
	const node3 = LeafNode.init(3, 0.8);

	try priority_queue.append(node1);
	try priority_queue.append(node2);
	try priority_queue.append(node3);

	std.debug.print("{any}\n", .{priority_queue.lowest_probability()});

	while (priority_queue.queue.items.len > 0) {
		const node1 = priority_queue.pop_lowest_probability();
		const node2 = priority_queue.pop_lowest_probability();
	}
}
