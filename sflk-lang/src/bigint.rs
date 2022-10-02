//! Implementation of big integers (integers that can be
//! as big as we want) so that SFLK integers can be free
//! from the usual size limitations of 64 bits.
//!
//! There are a lot of stuff here that can be optimized.

use std::cmp::Ordering;

type Digit = u8;

const BASE: u64 = 256;

/// An unsigned big integer.
/// The base is `BASE`, the most significants digits are at the back.
#[derive(Clone, Debug)]
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
			},
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

	fn from_string_base10(string: &str) -> Result<BigUint, ()> {
		let mut value = BigUint::zero();
		let ten = BigUint::from_u64(10);
		for c in string.chars() {
			if c.is_ascii_digit() {
				value.multiply_in_place(&ten);
				value.add_in_place(&BigUint::from_u64(c as u64 - '0' as u64))
			} else {
				return Err(());
			}
		}
		Ok(value)
	}

	fn to_string_base10(&self) -> String {
		if self.is_zero() {
			return String::from("0");
		}
		let mut string = String::new();
		let mut value = self.clone();
		let ten = BigUint::from_u64(10);
		while !value.is_zero() {
			let (q, r) = value.euclidian_divide(&ten);
			string = format!("{}", r.to_u64()) + &string;
			value = q;
		}
		string
	}

	fn index_of_higest_nonzero_digit(&self) -> Option<usize> {
		for i in (0..self.digits.len()).rev() {
			if self.digits[i] != 0 {
				return Some(i);
			}
		}
		None
	}

	fn is_zero(&self) -> bool {
		for &digit in self.digits.iter() {
			if digit != 0 {
				return false;
			}
		}
		true
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
		let other_higest = match other.index_of_higest_nonzero_digit() {
			Some(index) => index,
			None => return, // `other` is zero
		};
		let self_higest = match self.index_of_higest_nonzero_digit() {
			Some(index) => index,
			None => panic!("`self` is smaller than `other`"), // `self` is zero but `other` is not
		};
		while i <= other_higest || carry > 0 {
			assert!(
				i <= self_higest || carry == 0,
				"`self` is smaller than `other`"
			);
			let self_digit = self.digits[i] as u64; // h
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
	/// The output is (quotient, remainder).
	#[must_use]
	fn euclidian_divide(&self, denominator: &BigUint) -> (BigUint, BigUint) {
		// Long division.
		assert!(!denominator.is_zero(), "division by zero");
		let mut quoient = BigUint::zeros(self.digits.len());
		let mut remainder = self.clone();
		for i in 1..=self.digits.len() {
			let x = BigUint {
				digits: remainder.digits[(self.digits.len() - i)..].into(),
			};
			// `x` is the top number of the incomming subtraction
			// with the bigest multiple of `denominator` that is
			// strictly smaller than `x`.
			let (qx, rx) = if &x < denominator {
				(0, x)
			} else {
				let mut power_of_two = 2;
				let mut qx = BASE / power_of_two;
				let mut qx_mult_d = BigUint::from_u64(qx).multiply(denominator);
				loop {
					power_of_two *= 2;
					if x >= qx_mult_d {
						let maybe_rx = x.subtract(&qx_mult_d);
						if &maybe_rx < denominator {
							break (qx, maybe_rx);
						}
						qx += BASE / power_of_two;
					} else {
						qx -= BASE / power_of_two;
					}
					qx_mult_d = BigUint::from_u64(qx).multiply(denominator);
				}
			};
			assert!(qx < BASE);
			match rx.index_of_higest_nonzero_digit() {
				Some(rx_highest) => {
					for j in 0..=rx_highest {
						remainder.digits[self.digits.len() - i + j] = rx.digits[j];
					}
					for j in (self.digits.len() - i + rx_highest + 1)..remainder.digits.len() {
						remainder.digits[j] = 0;
					}
				},
				None => {
					for j in (self.digits.len() - i)..remainder.digits.len() {
						remainder.digits[j] = 0;
					}
				},
			}
			let qi = quoient.digits.len() - i;
			quoient.digits[qi] = qx as Digit;
		}
		(quoient, remainder)
	}
}

/// A signed big integer.
#[derive(Clone, Debug)]
pub struct BigSint {
	biguint: BigUint,
	/// This can be whatever if `biguint` is zero.
	is_negative: bool,
}

impl PartialEq for BigSint {
	fn eq(&self, other: &Self) -> bool {
		self.biguint == other.biguint
			&& (self.is_negative == other.is_negative || self.biguint.is_zero())
	}
}

impl Eq for BigSint {}

impl PartialOrd for BigSint {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for BigSint {
	fn cmp(&self, other: &BigSint) -> Ordering {
		if self.biguint.is_zero() && other.biguint.is_zero() {
			return Ordering::Equal;
		}
		let unsigned_cmp = self.biguint.cmp(&other.biguint);
		match (self.is_negative, other.is_negative) {
			(false, false) => unsigned_cmp,
			(true, true) => unsigned_cmp.reverse(),
			(true, false) => Ordering::Less,
			(false, true) => Ordering::Greater,
		}
	}
}

impl BigSint {
	pub fn zero() -> BigSint {
		BigSint { biguint: BigUint::zero(), is_negative: false }
	}

	pub fn from_u64(value: u64) -> BigSint {
		BigSint {
			biguint: BigUint::from_u64(value),
			is_negative: false,
		}
	}

	pub fn from_i64(value: i64) -> BigSint {
		BigSint {
			biguint: BigUint::from_u64(value.abs() as u64),
			is_negative: value < 0,
		}
	}

	pub fn to_i64(&self) -> i64 {
		i64::try_from(self.biguint.to_u64()).unwrap() * if self.is_negative { -1 } else { 1 }
	}

	pub fn from_bool(value: bool) -> BigSint {
		if value {
			BigSint::from_u64(1)
		} else {
			BigSint::zero()
		}
	}

	fn from_biguint(biguint: BigUint) -> BigSint {
		BigSint { biguint, is_negative: false }
	}

	pub fn from_string_base10(string: &str) -> Result<BigSint, ()> {
		Ok(BigSint::from_biguint(BigUint::from_string_base10(string)?))
	}

	pub fn to_string_base10(&self) -> String {
		String::from(if self.is_negative {"-"} else {""}) + &self.biguint.to_string_base10()
	}

	pub fn is_zero(&self) -> bool {
		self.biguint.is_zero()
	}

	fn add_in_place_ex(&mut self, other: &BigSint, flip_other_sign: bool) {
		let other_is_negative = if flip_other_sign {
			!other.is_negative
		} else {
			other.is_negative
		};
		match (self.is_negative, other_is_negative) {
			(false, false) | (true, true) => self.biguint.add_in_place(&other.biguint),
			(false, true) if self.biguint >= other.biguint => {
				self.biguint.subtract_in_place(&other.biguint);
			},
			(false, true) => {
				self.biguint = other.biguint.subtract(&self.biguint);
				self.is_negative = true;
			},
			(true, false) if self.biguint > other.biguint => {
				self.biguint.subtract_in_place(&other.biguint);
			},
			(true, false) => {
				self.biguint = other.biguint.subtract(&self.biguint);
				self.is_negative = false;
			},
		}
	}

	fn add_in_place(&mut self, other: &BigSint) {
		self.add_in_place_ex(other, false);
	}

	#[must_use]
	pub fn add(&self, other: &BigSint) -> BigSint {
		let mut res = self.clone();
		res.add_in_place(other);
		res
	}

	fn subtract_in_place(&mut self, other: &BigSint) {
		self.add_in_place_ex(other, true);
	}

	#[must_use]
	pub fn subtract(&self, other: &BigSint) -> BigSint {
		let mut res = self.clone();
		res.subtract_in_place(other);
		res
	}

	fn multiply_in_place(&mut self, other: &BigSint) {
		let res = self.multiply(other);
		*self = res;
	}

	#[must_use]
	pub fn multiply(&self, other: &BigSint) -> BigSint {
		let biguint = self.biguint.multiply(&other.biguint);
		BigSint {
			biguint,
			is_negative: self.is_negative ^ other.is_negative,
		}
	}

	/// Here, `self` is the numerator.
	/// The output is (quotient, remainder).
	#[must_use]
	pub fn euclidian_divide(&self, denominator: &BigSint) -> (BigSint, BigSint) {
		assert!(
			!denominator.is_negative,
			"euclidian division by a negative number"
		);
		let (q, r) = self.biguint.euclidian_divide(&denominator.biguint);
		(
			BigSint { biguint: q, is_negative: self.is_negative },
			BigSint::from_biguint(r),
		)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn zero_is_zero() {
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
			assert_eq!(
				big_a.add(&big_b).to_u64(),
				a + b,
				"results in {} + {}",
				a,
				b
			);
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
			(98866571, 98866570),
			(18446744073709551614, 1),
			(18446744073709551614, 435671),
			(18446744073709551614, 98866571),
			(18446744073709551614, 184467440737095),
			(18446744073709551614, 18446744073709551613),
			(18446744073709551614, 18446744073709551614),
		];
		for &(a, b) in pairs {
			assert!(a.checked_sub(b).is_some());
			let big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			assert_eq!(
				big_a.subtract(&big_b).to_u64(),
				a - b,
				"results in {} - {}",
				a,
				b
			);
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
			assert_eq!(
				big_a.multiply(&big_b).to_u64(),
				a * b,
				"results in {} * {}",
				a,
				b
			);
		}
	}

	#[test]
	fn compare() {
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
			assert_eq!(
				big_a.cmp(&big_b),
				a.cmp(&b),
				"results in comparions between {} and {}",
				a,
				b
			);
		}
	}

	#[test]
	fn divide() {
		let pairs: &[(u64, u64)] = &[
			(0, 1),
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
			let (q, r) = ((a / b), (a % b));
			let big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			let (big_q, big_r) = big_a.euclidian_divide(&big_b);
			assert_eq!(big_q.to_u64(), q, "quotients in {} / {}", a, b);
			assert_eq!(big_r.to_u64(), r, "remainders in {} / {}", a, b);
		}
	}
}
