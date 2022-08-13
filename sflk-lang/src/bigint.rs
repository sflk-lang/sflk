use std::cmp::Ordering;

type Digit = u8;

const BASE: u64 = 256;

/// An unsigned big integer.
/// The base is `BASE`, the most significants digits are at the end.
#[derive(Clone)]
struct BigUint {
	digits: Vec<Digit>,
}

impl PartialEq for BigUint {
	fn eq(&self, other: &Self) -> bool {
		let mut self_iter = self.digits.iter().cloned();
		let mut other_iter = other.digits.iter().cloned();
		loop {
			match (self_iter.next(), other_iter.next()) {
				(Some(a), Some(b)) if a != b => break false,
				(Some(_), Some(_)) => (),
				(Some(a), None) | (None, Some(a)) if a != 0 => break false,
				(Some(_), None) | (None, Some(_)) => (),
				(None, None) => break true,
			}
		}
	}
}

impl Eq for BigUint {}

impl PartialOrd for BigUint {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for BigUint {
	fn cmp(&self, other: &BigUint) -> Ordering {
		// Is this readable enough ?
		let self_start = self.index_of_higest_nonzero_digit();
		let other_start = other.index_of_higest_nonzero_digit();
		let start = match (self_start, other_start) {
			(None, None) => return Ordering::Equal,
			(Some(_), None) => return Ordering::Greater,
			(None, Some(_)) => return Ordering::Less,
			(Some(a), Some(b)) => match a.cmp(&b) {
				Ordering::Greater => return Ordering::Greater,
				Ordering::Less => return Ordering::Less,
				Ordering::Equal => a,
			}
		};
		// The number of significant digits is the same.
		for i in (0..=start).rev() {
			match self.digits[i].cmp(&other.digits[i]) {
				Ordering::Greater => return Ordering::Greater,
				Ordering::Less => return Ordering::Less,
				Ordering::Equal => (),
			}
		}
		Ordering::Equal
	}
}

impl BigUint {
	fn zero() -> BigUint {
		BigUint { digits: Vec::new() }
	}

	/// Constructs a `BigUint` that has `len` zeros as its digits.
	fn zeros(len: usize) -> BigUint {
		BigUint {
			digits: Some(0).into_iter().cycle().take(len).collect(),
		}
	}

	fn from_u64(mut value: u64) -> BigUint {
		let mut digits: Vec<Digit> = Vec::new();
		while value > 0 {
			digits.push((value % BASE) as Digit);
			value /= BASE;
		}
		BigUint { digits }
	}

	fn to_u64(&self) -> u64 {
		let mut acc = 0u64;
		for &digit in self.digits.iter().rev() {
			acc = acc * BASE + digit as u64;
		}
		acc
	}

	fn index_of_higest_nonzero_digit(&self) -> Option<usize> {
		for i in (0..self.digits.len()).rev() {
			if self.digits[i] != 0 {
				return Some(i);
			}
		}
		None
	}

	fn add_in_place(&mut self, other: &BigUint) {
		let mut i = 0;
		let mut carry = 0;
		while i < self.digits.len() || i < other.digits.len() || carry > 0 {
			if i >= self.digits.len() {
				self.digits.push(0);
			}
			let self_digit = self.digits[i] as u64;
			let other_digit = *other.digits.get(i).unwrap_or(&0) as u64;
			let digit_sum = self_digit + other_digit + carry;
			self.digits[i] = (digit_sum % BASE) as Digit;
			carry = digit_sum / BASE;
			i += 1;
		}
	}

	#[must_use]
	fn add(&self, other: &BigUint) -> BigUint {
		let mut res = self.clone();
		res.add_in_place(other);
		res
	}

	/// Perform `self = self - other` if `self >= other`.
	fn subtract_in_place(&mut self, other: &BigUint) {
		let mut i = 0;
		let mut carry = 0;
		while i < other.digits.len() || carry > 0 {
			assert!(
				i < self.digits.len() || carry == 0,
				"`self` is smaller than `other`"
			);
			let self_digit = self.digits[i] as u64;
			let other_digit = *other.digits.get(i).unwrap_or(&0) as u64;
			let mut top_digit = self_digit;
			let bottom_digit = other_digit + carry;
			carry = 0;
			if top_digit < bottom_digit {
				top_digit += BASE;
				carry = 1;
			}
			self.digits[i] = (top_digit - bottom_digit) as Digit;
			i += 1;
		}
	}

	#[must_use]
	fn subtract(&self, other: &BigUint) -> BigUint {
		let mut res = self.clone();
		res.subtract_in_place(other);
		res
	}

	fn multiply_in_place(&mut self, other: &BigUint) {
		let res = self.multiply(other);
		*self = res;
	}

	#[must_use]
	fn multiply(&self, other: &BigUint) -> BigUint {
		// https://en.wikipedia.org/wiki/Multiplication_algorithm#Long_multiplication
		let mut res = BigUint::zeros(self.digits.len() + other.digits.len());
		for other_i in 0..other.digits.len() {
			let mut carry = 0;
			for self_i in 0..self.digits.len() {
				let self_digit = self.digits[self_i] as u64;
				let other_digit = other.digits[other_i] as u64;
				let mut new_digit = res.digits[self_i + other_i] as u64;
				new_digit += self_digit * other_digit + carry;
				res.digits[self_i + other_i] = (new_digit % BASE) as Digit;
				carry = new_digit / BASE;
			}
			res.digits[self.digits.len() + other_i] = carry as Digit;
		}
		res
	}

	/// Here, `self` is the numerator.
	/// The output is (quotient, remainder)
	#[must_use]
	fn euclidian_divide(&self, denominator: &BigUint) -> (BigUint, BigUint) {
		let mut quoient = BigUint::zeros(self.digits.len());
		let mut remainder = self.clone();
		for i in 1..=self.digits.len() {
			let x = BigUint {
				digits: remainder.digits[self.digits.len()-i..].into()
			};
			todo!();
		}
		(quoient, remainder)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn zero() {
		let big_zero = BigUint::zero();
		assert_eq!(big_zero.to_u64(), 0);
	}

	#[test]
	fn u64_to_big_to_u64() {
		let values = [0, 1, 71, 255, 256, 257, 2345671, 18446744073709551615];
		for value in values {
			let big = BigUint::from_u64(value);
			assert_eq!(big.to_u64(), value);
		}
	}

	#[test]
	fn add() {
		let pairs: &[(u64, u64)] = &[
			(0, 0),
			(255, 1),
			(1, 255),
			(256, 256),
			(435671, 98866571),
			(18446744073709551614, 1),
		];
		for &(a, b) in pairs {
			assert!(a.checked_add(b).is_some());
			let big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			assert_eq!(big_a.add(&big_b).to_u64(), a + b);
		}
	}

	#[test]
	fn subtract() {
		let pairs: &[(u64, u64)] = &[
			(0, 0),
			(1, 0),
			(1, 1),
			(255, 45),
			(256, 255),
			(256, 256),
			(98866571, 435671),
			(18446744073709551614, 1),
			(18446744073709551614, 435671),
			(18446744073709551614, 98866571),
			(18446744073709551614, 184467440737095),
		];
		for &(a, b) in pairs {
			assert!(a.checked_sub(b).is_some());
			let big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			assert_eq!(big_a.subtract(&big_b).to_u64(), a - b);
		}
	}

	#[test]
	fn multiply() {
		let pairs: &[(u64, u64)] = &[
			(0, 0),
			(1, 0),
			(1, 1),
			(255, 45),
			(256, 255),
			(256, 256),
			(2, 256),
			(98866571, 435671),
			(435671, 98866571),
			(18446744073709551614, 1),
			(1, 18446744073709551614),
		];
		for &(a, b) in pairs {
			assert!(a.checked_mul(b).is_some());
			let big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			assert_eq!(big_a.multiply(&big_b).to_u64(), a * b);
		}
	}

	#[test]
	fn comparison() {
		let pairs: &[(u64, u64)] = &[
			(0, 0),
			(1, 0),
			(1, 1),
			(1, 18446744073709551614),
			(255, 45),
			(256, 255),
			(256, 256),
			(435671, 435671),
			(435671, 98866571),
			(98866571, 435671),
			(98866571, 98866571),
			(184467440737095, 184467440737095),
			(18446744073709551614, 1),
			(18446744073709551614, 435671),
			(18446744073709551614, 98866571),
			(18446744073709551614, 184467440737095),
			(18446744073709551614, 18446744073709551614),
		];
		for &(a, b) in pairs {
			let big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			assert_eq!(big_a.cmp(&big_b), a.cmp(&b));
		}
	}
}
