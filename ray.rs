use std::fs::File;
use std::io::Write;
use std::ops;

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
		let line = format!("{} {} {}\n", 255.999*self.x, 255.999*self.y, 255.999*self.z);
		f.write_all(line.as_bytes());
		()
	}
}

impl ops::Add<Vector> for Vector {
	type Output = Vector;

	fn add(self, rhs: Vector) -> Vector {
		Vector::new(self.x+rhs.x, self.y+rhs.y, self.z+rhs.z)
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
}

fn main() -> std::io::Result<()> {
	let aspect_ratio: f64 = 16.0 / 9.0;
	let image_width: i32 = 400;
	let image_height: i32 = ((image_width as f64) / aspect_ratio) as i32;

	let mut f = File::create("image.ppm")?;
	f.write_all(format!("P3\n{} {}\n255\n", image_width, image_height).as_bytes());

	for i in 0..image_width {
		for j in 0..image_height {
			let r: f64 = (i as f64) / (image_width as f64 - 1.0);
			let g: f64 = (j as f64) / (image_height as f64 - 1.0);
			let b: f64 = 0.0;
			let pixel = Color::new(r, g, b);
			pixel.write_color(&f);
		}
	}

	let direction = Vector::new(1.0, 2.0, 4.0);
	let origin = Point::new(0.0, 0.0, 0.0);

	println!("{:#?}", direction);
	println!("{:#?}", origin);

	Ok(())
}
