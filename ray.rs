use std::ops;

// #[derive(Debug)]
// struct Point {
// 	x: f64,
// 	y: f64,
// 	z: f64,
// }

#[derive(Debug)]
struct Vector {
	x: f64,
	y: f64,
	z: f64,
}

fn new_vector(x: f64, y: f64, z: f64) -> Vector {
	return Vector{
		x: x,
		y: y,
		z: z,
	}
}

impl ops::Add<Vector> for Vector {
	type Output = Vector;

	fn add(self: Vector, rhs: Vector) -> Vector {
		new_vector(self.x+rhs.x, self.y+rhs.y, self.z+rhs.z)
	}
}

impl ops::Mul<f64> for Vector {
	type Output = Vector;

	fn mul(self: Vector, rhs: f64) -> Vector {
		new_vector(self.x*rhs, self.y*rhs, self.z*rhs)
	}
}

impl ops::Mul<Vector> for f64 {
	type Output = Vector;

	fn mul(self: f64, rhs: Vector) -> Vector {
		new_vector(self*rhs.x, self*rhs.y, self*rhs.z)
	}
}

type Point = Vector;

fn new_point(x: f64, y: f64, z: f64) -> Point {
	return Point{
		x: x,
		y: y,
		z: z,
	}
}

#[derive(Debug)]
struct Ray {
	origin: Point,
	direction: Vector,
}

fn new_ray(origin: Point, direction: Vector) -> Ray {
	return Ray{
		origin: origin,
		direction: direction,
	}
}

impl Ray {
	fn at(self, t: f64) -> Point {
		self.origin + t*self.direction
	}
}

fn main() {
	// let aspect_ratio: f32 = 16.0 / 9.0;

	// let image_width: i32 = 400;
	// let image_height: i32 = ((image_width as f32) / aspect_ratio) as i32;

	let image_width: i32 = 256;
	let image_height: i32 = 256;

	println!("P3\n{} {}\n255", image_width, image_height);

	// for i in 0..image_width {
	// 	for j in 0..image_height {
	// 		let r: i32 = (i*255)/image_width;
	// 		let g: i32 = (j*255)/image_height;
	// 		let b: i32 = 50;
	// 		// println!("{} {} {}", r, g, b);
	// 	}
	// }

	let origin = new_point(0.0, 0.0, 0.0);
	let direction = new_vector(1.0, 2.0, 4.0);
	let ray = new_ray(origin, direction);
	println!("{:#?}", ray);

	let at = ray.at(1.2);
	println!("{:#?}", at);
}
