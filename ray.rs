use std::ops;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug)]
struct Vector {
	x: f64,
	y: f64,
	z: f64,
}

impl Vector {
	fn new(x: f64, y: f64, z: f64) -> Vector {
		Vector {
			x: x,
			y: y,
			z: z,
		}
	}

	fn write_color(self, mut f: &File) {
		let r = (self.x*255.0) as i32;
		let g = (self.y*255.0) as i32;
		let b = (self.z*255.0) as i32;
		let ln = format!("{} {} {}\n", r, g, b);
		f.write_all(ln.as_bytes());
	}
}

impl ops::Mul<f64> for Vector {
	type Output = Vector;

	fn mul(self, rhs: f64) -> Vector {
		Vector::new(self.x*rhs, self.y*rhs, self.z*rhs)
	}
}

impl ops::Mul<Vector> for f64 {
	type Output = Vector;

	fn mul(self, rhs: Vector) -> Vector {
		Vector::new(self*rhs.x, self*rhs.y, self*rhs.z)
	}
}

impl ops::Add<Vector> for Vector {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		Vector::new(self.x+rhs.x, self.y+rhs.y, self.z+rhs.z)
	}
}

type Point = Vector;
type Color = Vector;

struct Ray {
	origin: Point,
	direction: Vector,
}

impl Ray {
	fn new(origin: Point, direction: Vector) -> Ray {
		Ray {
			origin: origin,
			direction: direction,
		}
	}

	fn at(self, t: f64) -> Point {
		self.origin + t*self.direction
	}
}

fn main() {
	// https://raytracing.github.io/books/RayTracingInOneWeekend.html

	let aspect_ratio: f64 = 16.0 / 9.0;

	let image_width: i32 = 400;
	let image_height: i32 = ((image_width as f64) / aspect_ratio) as i32;

	let mut image = File::create("image.ppm").expect("failed to create file");

	image.write_all(format!("P3\n{} {}\n255\n", image_width, image_height).as_bytes());

	let focal_length = 1.0;
	let viewport_height = 2.0;
	let viewport_width = viewport_height * (image_width as f64 / image_height as f64);
	let camera_center = Point::new(0.0, 0.0, 0.0);

	let viewport_u = Vector::new(viewport_width, 0.0, 0.0);
	let viewport_v = Vector::new(0.0, -viewport_height, 0.0);

	let pixel_delta_u = viewport_u / image_width;
	let pixel_delta_v = viewport_v / image_height;

	let viewport_upper_left = camera_center - Vector::new(0.0, 0.0, focal_length) - viewport_u/2 - viewport_v/2;
	let pixel00_loc = viewport_upper_left + 0.5*(pixel_delta_u + pixel_delta_v)

	for i in 0..image_width {
		for j in 0..image_height {
			// let r = (i as f64)/(image_width as f64);
			// let g = (j as f64)/(image_height as f64);
			// let b = 0.0;

			let pixel_center = pixel00_loc + (i * pixel_delta_u) + (j * pixel_delta_v);
			let ray_direction = pixel_center - camera_center;

			Color::new(r, g, b).write_color(&image);
		}
	}
}
