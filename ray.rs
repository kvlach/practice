use std::ops;

#[derive(Debug)]
struct Vector {
	x: f64,
	y: f64,
	z: f64,
}

fn new_vector(x: f64, y: f64, z: f64) -> Vector {
	Vector {
		x: x,
		y: y,
		z: z,
	}
}

impl Vector {
	fn write_color(self) {
		println!("{} {} {}", self.x*255.0, self.y*255.0, self.z*255.0)
	}
}

impl ops::Add<Vector> for Vector {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		new_vector(self.x+rhs.x, self.y+rhs.y, self.z+rhs.z)
	}
}

impl ops::Mul<f64> for Vector {
	type Output = Vector;

	fn mul(self, rhs: f64) -> Vector {
		new_vector(self.x*rhs, self.y*rhs, self.z*rhs)
	}
}

impl ops::Mul<Vector> for f64 {
	type Output = Vector;

	fn mul(self, rhs: Vector) -> Vector {
		new_vector(self*rhs.x, self*rhs.y, self*rhs.z)
	}
}

impl ops::Div<f64> for Vector {
	type Output = Vector;

	fn div(self, rhs: f64) -> Vector {
		self * (1.0/rhs)
	}
}

impl ops::Div<Vector> for f64 {
	type Output = Vector;

	fn div(self, rhs: Vector) -> Vector {
		(1.0/self) * rhs
	}
}

type Point = Vector;

fn new_point(x: f64, y: f64, z: f64) -> Point {
	Point {
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
	Ray {
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
	let aspect_ratio: f64 = 16.0 / 9.0;

	let width: i32 = 400;
	let height: i32 = ((width as f64) / aspect_ratio) as i32;

	println!("P3\n{} {}\n255", width, height);

	for i in 0..width {
		for j in 0..height {
			let color = new_vector((i as f64)/(width as f64-1.0), (j as f64)/(height as f64-1.0), 0.0);
			color.write_color();
		}
	}

	let origin = new_point(0.0, 0.0, 0.0);
	let direction = new_vector(1.0, 2.0, 4.0);
	let ray = new_ray(origin, direction);
	// println!("{:#?}", ray);

	let at = ray.at(1.2);
	// println!("{:#?}", at);
}
