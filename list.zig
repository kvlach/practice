const std = @import("std");

const Item = struct {
    val: i32,
    prev: ?*Item = null,
    next: ?*Item = null,

    fn new(allocator: std.mem.Allocator, val: i32) !*Item {
        var it = try allocator.create(Item);
        it.val = val;
        it.prev = null;
        it.next = null;
        return it;
    }

    fn print(self: *Item) void {
        std.debug.print("{d}\n", .{self.val});
    }
};

const ListErrors = error{
    OutOfBounds,
};

const List = struct {
    len: usize = 0,
    head: ?*Item = null,
    tail: ?*Item = null,
    allocator: std.mem.Allocator,

    fn new(allocator: std.mem.Allocator) !*List {
        var ls = try allocator.create(List);
        ls.allocator = allocator;
        ls.len = 0;
        ls.head = null;
        ls.tail = null;
        return ls;
    }

    fn append(self: *List, val: i32) !void {
        var it = try Item.new(self.allocator, val);

        if (self.len == 0) {
            self.head = it;
            self.tail = it;
        } else {
            self.tail.?.next = it;
            it.prev = self.tail;
            self.tail = it;
        }

        self.len += 1;
    }

    // fn insert(self: *List, index: usize, val: i32) !void {
    //     var it = try Item.new(self.allocator, val);

    //     self.len += 1;
    // }

    fn get(self: *List, index: usize) !*Item {
        if (index >= self.len) {
            return ListErrors.OutOfBounds;
        }

        var it = self.head;
        for (0..index) |_| {
            it = it.?.next;
        }
        return it.?;
    }

    fn set(self: *List, index: usize, val: i32) !void {
        if (index >= self.len) {
            return ListErrors.OutOfBounds;
        }

        var it = self.head;
        for (0..index) |_| {
            it = it.?.next;
        }
        it.?.val = val;
    }

    fn delete(self: *List, index: usize) !i32 {
        if (index >= self.len) {
            return ListErrors.OutOfBounds;
        }

        var it = self.head.?;
        for (0..index) |_| {
            it = it.next.?;
        }

        if (self.head == it) {
            self.head = it.next;
            self.head.?.prev = null;
        } else if (self.tail == it) {
            self.tail = it.prev;
            self.tail.?.next = null;
        } else {
            it.prev.?.next = it.next;
            it.next.?.prev = it.prev;
        }

        return it.val;
    }

    fn debug(self: *List) void {
        std.debug.print("{}\n", .{self});
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
        std.debug.print("\n", .{});
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var ls = try List.new(allocator);
    try ls.append(5);
    try ls.append(3);
    ls.print();

    try ls.set(1, 91);
    try ls.append(0);
    try ls.append(7);
    const it = try ls.get(1);
    it.print();
    _ = try ls.delete(0);
    ls.print();
}
