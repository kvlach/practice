fn main() {
	let image_width: i32 = 256;
	let image_height: i32 = 256;

	println!("P3\n{} {}\n255", image_width, image_height);

	for j in 0..image_height {
		for i in 0..image_width {
			let r = 255*i / image_width;
			// let r = (image_width-1) / (i+1);
			let g = 255*j / image_height;
			let b = 50;
			println!("{} {} {}", r, g, b);
		}
	}
}
