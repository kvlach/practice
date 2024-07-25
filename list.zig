const std = @import("std");

const Item = struct {
    val: i32,
    prev: ?*Item = null,
    next: ?*Item = null,

    fn new(allocator: std.mem.Allocator, val: i32) *Item {
        const it = try allocator.create(Item);
        it.* = Item{ .val = val };
        return it;
    }

    fn print(self: *Item) void {
        std.debug.print("{d}\n", .{self.val});
    }

    fn debug(self: *Item) void {
        std.debug.print("{}\n", .{self});
    }
};

const List = struct {
    len: usize = 0,
    head: ?*Item = null,
    tail: ?*Item = null,
    allocator: std.mem.Allocator,

    fn new(allocator: std.mem.Allocator) *List {
        const ls = try allocator.create(List);
        ls.allocator = allocator;
        return ls;
    }

    fn debug(self: *List) void {
        std.debug.print("{}\n", .{self});
    }

    fn append(self: *List, val: i32) void {
        var it = Item.new(val);

        if (self.len == 0) {
            it.print();
            it.debug();
            // std.debug.print("{}\n", .{self});
            self.debug();
            self.head = it;
            self.tail = it;
        } else {
            self.tail.?.next = it;
            it.prev = self.tail;
            self.tail = it;
        }

        self.len += 1;
    }

    fn print(self: *List) void {
        if (self.len == 0) {
            return;
        }

        var it = self.head;
        while (it != null) {
            std.debug.print("{d} ", .{it.?.val});
            it = it.?.next;
        }
    }
};

pub fn main() void {
    var arena = std.heap.ArenaAllocator();
    arena.init(std.heap.page_allocator);
    var allocator = arena.allocator();

    var ls = List.new(allocator);
    ls.append(3);
    ls.print();
}
