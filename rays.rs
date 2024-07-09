use std::ops;

#[derive(Debug)]
struct Vector {
	x: i32,
	y: i32,
	z: i32,
}

fn new_vector(x: i32, y: i32, z: i32) -> Vector {
	return Vector {
		x: x,
		y: y,
		z: z,
	}
}

impl ops::Add<Vector> for Vector {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		return new_vector(self.x+rhs.x, self.y+rhs.y, self.z+rhs.z)
	}
}

impl ops::Sub<Vector> for Vector {
	type Output = Vector;

	fn sub(self, rhs: Vector) -> Vector {
		return new_vector(self.x-rhs.x, self.y-rhs.y, self.z-rhs.z)
	}
}

fn main(){
	let aspect_ratio: f32 = 16.0 / 9.0;
	let image_height: i32 = 200;
	let image_width: i32 = (image_height as f32 / aspect_ratio) as i32;

	println!("P3\n{} {}\n255", image_width, image_height);

	for i in 0..image_height {
		for j in 0..image_width {
			let g = (i*255) / (image_height-1);
			let r = (j*255) / (image_width-1);
			let b = 50;

			// println!("{} {} {}", r, g, b);
		}
	}

	let vec = new_vector(0, 0, 2) + new_vector(1, 0, 3) - new_vector(1, 2, 0);
	println!("{:?}", vec)
}
