use std::ops;
use std::fs::File;
use std::io::Write;

#[derive(Debug, Copy, Clone)]
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

	fn write(self, mut f: &File) {
		let r = (self.x * 255.0) as i32;
		let g = (self.y * 255.0) as i32;
		let b = (self.z * 255.0) as i32;
		let ln = format!("{} {} {}\n", r, g, b);
		f.write_all(ln.as_bytes());
	}

	fn length(self) -> f64 {
		(self.x*self.x + self.y*self.y + self.z*self.z).sqrt()
	}

	fn unit(self) -> Vector {
		self / self.length()
	}

	fn dot(self, rhs: Vector) -> f64 {
		self.x*rhs.x + self.y*rhs.y + self.z*rhs.z
	}
}

type Point = Vector;
type Color = Vector;

// Addition

impl ops::Add<f64> for Vector {
	type Output = Vector;

	fn add(self, rhs: f64) -> Vector {
		Vector::new(self.x+rhs, self.y+rhs, self.z+rhs)
	}
}

impl ops::Add<Vector> for f64 {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		rhs + self
	}
}

impl ops::Add<i32> for Vector {
	type Output = Vector;

	fn add(self, rhs: i32) -> Vector {
		self + (rhs as f64)
	}
}

impl ops::Add<Vector> for i32 {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		(self as f64) + rhs
	}
}

impl ops::Add<Vector> for Vector {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		Vector::new(self.x+rhs.x, self.y+rhs.y, self.z+rhs.z)
	}
}

// Subtraction

impl ops::Sub<f64> for Vector {
	type Output = Vector;

	fn sub(self, rhs: f64) -> Vector {
		Vector::new(self.x-rhs, self.y-rhs, self.z-rhs)
	}
}

impl ops::Sub<Vector> for f64 {
	type Output = Vector;

	fn sub(self, rhs: Vector) -> Vector {
		rhs - self
	}
}

impl ops::Sub<i32> for Vector {
	type Output = Vector;

	fn sub(self, rhs: i32) -> Vector {
		self - (rhs as f64)
	}
}

impl ops::Sub<Vector> for i32 {
	type Output = Vector;

	fn sub(self, rhs: Vector) -> Vector {
		rhs - (self as f64)
	}
}

impl ops::Sub<Vector> for Vector {
	type Output = Vector;

	fn sub(self, rhs: Vector) -> Vector {
		Vector::new(self.x-rhs.x, self.y-rhs.y, self.z-rhs.z)
	}
}

// Multiplication

impl ops::Mul<f64> for Vector {
	type Output = Vector;

	fn mul(self, rhs: f64) -> Vector {
		Vector::new(self.x*rhs, self.y*rhs, self.z*rhs)
	}
}

impl ops::Mul<Vector> for f64 {
	type Output = Vector;

	fn mul(self, rhs: Vector) -> Vector {
		rhs * self
	}
}

impl ops::Mul<i32> for Vector {
	type Output = Vector;

	fn mul(self, rhs: i32) -> Vector {
		self * (rhs as f64)
	}
}

impl ops::Mul<Vector> for i32 {
	type Output = Vector;

	fn mul(self, rhs: Vector) -> Vector {
		rhs * (self as f64)
	}
}

// Division

impl ops::Div<f64> for Vector {
	type Output = Vector;

	fn div(self, rhs: f64) -> Vector {
		Vector::new(self.x/rhs, self.y/rhs, self.z/rhs)
	}
}

impl ops::Div<i32> for Vector {
	type Output = Vector;

	fn div(self, rhs: i32) -> Vector {
		self / (rhs as f64)
	}
}

#[derive(Debug, Copy, Clone)]
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

	fn color(self) -> Color {
		let t = hit_sphere(Point::new(0.0, 0.0, -1.0), 0.5, self);
		if t > 0.0 {
			let N = (self.at(t) - Vector::new(0.0, 0.0, -1.0)).unit();
			return 0.5*Color::new(N.x+1.0, N.y+1.0, N.z+1.0);
		}

		let unit_direction = self.direction.unit();
		let a = 0.5 * (unit_direction.y + 1.0);
		(1.0-a) * Color::new(1.0, 1.0, 1.0) + a*Color::new(0.5, 0.7, 1.0)
	}
}

fn hit_sphere(center: Point, radius: f64, r: Ray) -> f64 {
	let oc = center - r.origin;
	let a = r.direction.dot(r.direction);
	let b = -2.0 * r.direction.dot(oc);
	let c = oc.dot(oc) - radius*radius;
	let discriminant = b*b - 4.0*a*c;

	if discriminant < 0.0 {
		return -1.0;
	}
	return (-b - discriminant.sqrt()) / (2.0 * a);
}

fn main() {
	let mut image = File::create("image.ppm").expect("failed to create file");

	let image_width = 256;
	let image_height = 256;

	let focal_length = 1.0;
	let viewport_height = 2.0;
	let viewport_width = viewport_height * ((image_width as f64) / (image_height as f64));
	let camera_center = Point::new(0.0, 0.0, 0.0);

	let viewport_u = Vector::new(viewport_width, 0.0, 0.0);
	let viewport_v = Vector::new(0.0, -viewport_height, 0.0);

	let pixel_delta_u = viewport_u / image_width;
	let pixel_delta_v = viewport_v / image_height;

	let viewport_upper_left = camera_center - Vector::new(0.0, 0.0, focal_length) - viewport_u/2 - viewport_v/2;
	let pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v);

	image.write_all(format!("P3\n{} {}\n255\n", image_width, image_height).as_bytes());

	for i in 0..image_width {
		for j in 0..image_height {
			let pixel_center = pixel00_loc + (i * pixel_delta_u) + (j * pixel_delta_v);
			let ray_direction = pixel_center - camera_center;
			let pixel_color = Ray::new(camera_center, ray_direction).color();
			pixel_color.write(&image);
		}
	}
}
