//! Implementation of big integers (integers that can be
//! arbitrarly big) so that SFLK integers can be free
//! from the usual 64 bits size limitation.
//!
//! There are a lot of stuff here that can be optimized.
//! There are also a lot of unit tests to write.

use std::cmp::Ordering;

/// The type of one digit.
///
/// It does not have to be `u8`, this is an arbitrary decision,
/// and an uneducated guess at that.
/// I heard Python uses digits that fit closely in 32-bits or something,
/// maybe a `u32` could be more appropriate.
/// TODO: Benchmark heavy math hapenning with different types for `Digit`.
type Digit = u8;

/// The base should be of type `u64` and is the number of
/// values that the `Digit` type can represent.
const BASE: u64 = Digit::MAX as u64 + 1;

/// An unsigned big integer.
/// The base is `BASE`, the most significants digits are at the back.
/// There can be leading zeros.
/// There can be no digits at all (in such case the represented value is zero).
///
/// Assuming the `BASE` is 256, the integer 256 can be represented as
/// `[0, 1]` or `[0, 1, 0]` or `[0, 1, 0, 0]` etc.
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

#[derive(Debug)]
pub struct DoesNotFit;

impl TryFrom<&BigUint> for u64 {
	type Error = DoesNotFit;

	fn try_from(value: &BigUint) -> Result<u64, DoesNotFit> {
		let mut acc = 0u64;
		for &digit in value.digits.iter().rev() {
			acc = acc.checked_mul(BASE).ok_or(DoesNotFit)?;
			acc += digit as u64;
		}
		Ok(acc)
	}
}

impl From<u64> for BigUint {
	fn from(mut value: u64) -> BigUint {
		let mut digits: Vec<Digit> = Vec::new();
		while value > 0 {
			digits.push((value % BASE) as Digit);
			value /= BASE;
		}
		BigUint { digits }
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

	fn from_string_base10(string: &str) -> Result<BigUint, ()> {
		let mut value = BigUint::zero();
		let ten = BigUint::from(10);
		for c in string.chars() {
			if c.is_ascii_digit() {
				value.multiply_in_place(&ten);
				value.add_in_place(&BigUint::from(c as u64 - '0' as u64))
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
		let ten = BigUint::from(10);
		while !value.is_zero() {
			let (q, r) = value.euclidian_divide(&ten);
			string = format!("{}", u64::try_from(&r).unwrap()) + &string;
			value = q;
		}
		string
	}

	/// Returns `None` iff `self` is zero.
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
			None => return, // `other` is zero.
		};
		let self_higest = match self.index_of_higest_nonzero_digit() {
			Some(index) => index,
			None => panic!("`self` is smaller than `other`"), // `self` is zero but `other` is not.
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
		// See [https://en.wikipedia.org/wiki/Multiplication_algorithm#Long_multiplication].
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
		let mut quotient = BigUint::zeros(self.digits.len());
		let mut remainder = self.clone();
		for i in 1..=self.digits.len() {
			// Each iteration, a bigger slice of the digits of `remainder` is considered as `x`.
			// Precisely, one digit bigger than the last considered slice (that was replaced by
			// the result of the last subtraction if any). This is pretty much what we do in a
			// pen and paper division when we "bring down" the next digit of the numerator next to
			// the last subrcation result (at least in France that is what we do).
			let x = BigUint {
				digits: remainder.digits[(self.digits.len() - i)..].into(),
			};
			// `x` is the top number of the incomming subtraction
			// with the bigest multiple of `denominator` (`qx` * `denominator`)
			// that is strictly smaller than `x`.
			// `x - qx * denominator == rx`.
			let (qx, rx) = if &x < denominator {
				(0, x)
			} else {
				// That bigest multiple of `denominator` that is
				// strictly smaller than `x` is searched for in the same
				// way as binary search.
				let mut power_of_two = 2;
				let mut qx = BASE / power_of_two;
				let mut qx_times_denominator = BigUint::from(qx).multiply(denominator);
				loop {
					power_of_two *= 2;
					if x >= qx_times_denominator {
						let maybe_rx = x.subtract(&qx_times_denominator);
						if &maybe_rx < denominator {
							break (qx, maybe_rx);
						}
						qx += BASE / power_of_two;
					} else {
						qx -= BASE / power_of_two;
					}
					qx_times_denominator = BigUint::from(qx).multiply(denominator);
				}
			};
			assert!(qx < BASE);
			// The slice of `remainder` that was put in `x` at the beginning of this iteration
			// is now replaced by the result of the subtraction (`rx`).
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
			// `qi` is the next digit in the quotient.
			let qi = quotient.digits.len() - i;
			quotient.digits[qi] = qx as Digit;
		}
		(quotient, remainder)
	}
}

#[cfg(test)]
mod test_big_uint {
	use super::*;

	#[test]
	fn zero_is_zero() {
		let big_zero = BigUint::zero();
		assert_eq!(u64::try_from(&big_zero).unwrap(), 0);
	}

	#[test]
	fn u64_to_big_to_u64() {
		let values = [0, 1, 71, 255, 256, 257, 2345671, 18446744073709551615];
		for value in values {
			let big = BigUint::from(value);
			assert_eq!(u64::try_from(&big).unwrap(), value);
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
			let big_a = BigUint::from(a);
			let big_b = BigUint::from(b);
			assert_eq!(
				u64::try_from(&big_a.add(&big_b)).unwrap(),
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
			let big_a = BigUint::from(a);
			let big_b = BigUint::from(b);
			assert_eq!(
				u64::try_from(&big_a.subtract(&big_b)).unwrap(),
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
			let big_a = BigUint::from(a);
			let big_b = BigUint::from(b);
			assert_eq!(
				u64::try_from(&big_a.multiply(&big_b)).unwrap(),
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
			let big_a = BigUint::from(a);
			let big_b = BigUint::from(b);
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
			let big_a = BigUint::from(a);
			let big_b = BigUint::from(b);
			let (big_q, big_r) = big_a.euclidian_divide(&big_b);
			assert_eq!(
				u64::try_from(&big_q).unwrap(),
				q,
				"quotients in {} / {}",
				a,
				b
			);
			assert_eq!(
				u64::try_from(&big_r).unwrap(),
				r,
				"remainders in {} / {}",
				a,
				b
			);
		}
	}
}

/// A signed big integer.
#[derive(Clone, Debug)]
pub struct BigSint {
	biguint: BigUint,
	/// The value zero does not have a specific sign, this can be whatever for zero.
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

impl TryFrom<&BigSint> for i64 {
	type Error = DoesNotFit;

	fn try_from(value: &BigSint) -> Result<i64, DoesNotFit> {
		let biguint_as_u64: u64 = (&value.biguint).try_into()?;
		let biguint_as_i64: i64 = biguint_as_u64.try_into().map_err(|_| DoesNotFit)?;
		Ok(biguint_as_i64 * if value.is_negative { -1 } else { 1 })
	}
}

impl From<u64> for BigSint {
	fn from(value: u64) -> BigSint {
		BigSint { biguint: BigUint::from(value), is_negative: false }
	}
}

impl From<i64> for BigSint {
	fn from(value: i64) -> BigSint {
		BigSint {
			biguint: BigUint::from(value.abs() as u64),
			is_negative: value < 0,
		}
	}
}

impl BigSint {
	pub fn zero() -> BigSint {
		BigSint { biguint: BigUint::zero(), is_negative: false }
	}

	pub fn from_bool(value: bool) -> BigSint {
		if value {
			BigSint::from(1i64)
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
		String::from(if self.is_negative { "-" } else { "" }) + &self.biguint.to_string_base10()
	}

	pub fn is_zero(&self) -> bool {
		self.biguint.is_zero()
	}

	pub fn is_negative(&self) -> bool {
		self.is_negative
	}

	fn negate_in_place(&mut self) {
		// Rust does not have a `bool::toggle()` method smh.
		self.is_negative = !self.is_negative;
	}

	fn abs(mut self) -> BigSint {
		self.is_negative = false;
		self
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
				// (+8) + (-3) = +5 = +(8 - 3)
				self.biguint.subtract_in_place(&other.biguint);
				self.is_negative = false;
			},
			(false, true) => {
				// (+3) + (-8) = -5 = -(8 - 3)
				self.biguint = other.biguint.subtract(&self.biguint);
				self.is_negative = true;
			},
			(true, false) if self.biguint > other.biguint => {
				// (-8) + (+3) = -5 = -(8 - 3)
				self.biguint.subtract_in_place(&other.biguint);
				self.is_negative = true;
			},
			(true, false) => {
				// (-3) + (+8) = +5 = +(8 - 3)
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

/// A big fraction (arbitrarly precise).
///
/// It must always be simplified.
#[derive(Clone, Debug)]
pub struct BigFraction {
	numerator: BigSint,
	/// The `denominator` must be positive when the fraction is simplified
	/// (so that this can be relied on to simplify code).
	/// The `denominator` must never be zero.
	/// The `denominator` can be whatever it wants when the `numerator` is zero
	/// (but still not zero and still positive).
	denominator: BigSint,
}

impl PartialEq for BigFraction {
	fn eq(&self, other: &Self) -> bool {
		(self.numerator.is_zero() && other.numerator.is_zero())
			|| (self.numerator == other.numerator && self.denominator == other.denominator)
	}
}

impl Eq for BigFraction {}

impl PartialOrd for BigFraction {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for BigFraction {
	fn cmp(&self, other: &BigFraction) -> Ordering {
		// (a/b) < (c/d) iff (a*d) < (c*b) assuming non-zero denominators.
		(self.numerator.multiply(&other.denominator))
			.cmp(&other.numerator.multiply(&self.denominator))
	}
}

impl BigFraction {
	pub fn numerator(&self) -> &BigSint {
		&self.numerator
	}

	pub fn denominator(&self) -> &BigSint {
		&self.denominator
	}
}

#[derive(Debug)]
pub struct NotAnInteger;

impl TryFrom<&BigFraction> for BigSint {
	type Error = NotAnInteger;

	fn try_from(value: &BigFraction) -> Result<BigSint, NotAnInteger> {
		if value.numerator.is_zero() {
			Ok(BigSint::zero())
		} else if value.denominator == BigSint::from(1u64) {
			Ok(value.numerator.clone())
		} else {
			Err(NotAnInteger)
		}
	}
}

impl From<u64> for BigFraction {
	fn from(value: u64) -> BigFraction {
		BigFraction::from_integers(BigSint::from(value), BigSint::from(1u64))
	}
}

impl From<i64> for BigFraction {
	fn from(value: i64) -> BigFraction {
		BigFraction::from_integers(BigSint::from(value), BigSint::from(1u64))
	}
}

impl From<BigSint> for BigFraction {
	fn from(value: BigSint) -> BigFraction {
		BigFraction::from_integers(value, BigSint::from(1u64))
	}
}

fn gcd(mut a: BigSint, mut b: BigSint) -> BigSint {
	if a < b {
		// We want `a` to be greater than `b`.
		gcd(b, a)
	} else {
		// Here `a` is greater than `b`.
		// See [https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclidean_algorithm].
		while !b.is_zero() {
			let a_mod_b = a.euclidian_divide(&b).1;
			(a, b) = (b, a_mod_b);
		}
		a
	}
}

impl BigFraction {
	pub fn zero() -> BigFraction {
		BigFraction {
			numerator: BigSint::from(0u64),
			denominator: BigSint::from(1u64),
		}
	}

	fn from_integers(numerator: BigSint, denominator: BigSint) -> BigFraction {
		let mut big_fraction = BigFraction { numerator, denominator };
		big_fraction.simplify_in_place();
		big_fraction
	}

	pub fn is_zero(&self) -> bool {
		self.numerator.is_zero()
	}

	fn simplify_in_place(&mut self) {
		// Ensure that the denominator is strictly positive.
		assert!(!self.denominator.is_zero());
		if self.denominator.is_negative() {
			self.numerator.negate_in_place();
			self.denominator.negate_in_place();
		}

		// Simplify by the GCD.
		let num_den_gcd = gcd(self.numerator.clone().abs(), self.denominator.clone().abs());
		let (new_numerator, zero) = self.numerator.euclidian_divide(&num_den_gcd);
		assert!(zero.is_zero());
		let (new_denominator, zero) = self.denominator.euclidian_divide(&num_den_gcd);
		assert!(zero.is_zero());
		self.numerator = new_numerator;
		self.denominator = new_denominator;
	}

	#[must_use]
	fn simplify(&self) -> BigFraction {
		let mut res = self.clone();
		res.simplify_in_place();
		res
	}

	fn is_negative(&self) -> bool {
		self.numerator.is_negative()
	}

	fn negate_in_place(&mut self) {
		self.numerator.negate_in_place()
	}

	fn add_in_place(&mut self, other: &BigFraction) {
		let res = self.add(other);
		*self = res;
	}

	#[must_use]
	pub fn add(&self, other: &BigFraction) -> BigFraction {
		let (a, b, c, d) = (
			&self.numerator,
			&self.denominator,
			&other.numerator,
			&other.denominator,
		);
		// (a/b) + (c/d) = (a*d)/(b*d) + (c*b)/(b*d) = (a*d + c*b)/(b*d)
		BigFraction::from_integers((a.multiply(&d)).add(&c.multiply(&b)), b.multiply(&d))
	}

	fn subtract_in_place(&mut self, other: &BigFraction) {
		let res = self.subtract(other);
		*self = res;
	}

	#[must_use]
	pub fn subtract(&self, other: &BigFraction) -> BigFraction {
		let (a, b, c, d) = (
			&self.numerator,
			&self.denominator,
			&other.numerator,
			&other.denominator,
		);
		// (a/b) - (c/d) = (a*d)/(b*d) - (c*b)/(b*d) = (a*d - c*b)/(b*d)
		BigFraction::from_integers((a.multiply(&d)).subtract(&c.multiply(&b)), b.multiply(&d))
	}

	fn multiply_in_place(&mut self, other: &BigFraction) {
		self.numerator.multiply_in_place(&other.numerator);
		// Both denominators are strictly positive so their product is strictly positive.
		self.denominator.multiply_in_place(&other.denominator);
		self.simplify_in_place();
	}

	#[must_use]
	pub fn multiply(&self, other: &BigFraction) -> BigFraction {
		BigFraction::from_integers(
			self.numerator.multiply(&other.numerator),
			self.denominator.multiply(&other.denominator),
		)
	}

	fn invert_in_place(&mut self) {
		assert!(!self.is_zero(), "invert of zero");
		std::mem::swap(&mut self.numerator, &mut self.denominator);
		self.simplify_in_place();
	}

	fn invert(&self) -> BigFraction {
		assert!(!self.is_zero(), "invert of zero");
		BigFraction::from_integers(self.denominator.clone(), self.numerator.clone())
	}

	fn divide_in_place(&mut self, other: &BigFraction) {
		self.multiply_in_place(&other.invert());
		self.simplify_in_place();
	}

	#[must_use]
	pub fn divide(&self, other: &BigFraction) -> BigFraction {
		self.multiply(&other.invert()).simplify()
	}
}

#[cfg(test)]
mod test_big_fraction {
	use super::*;

	#[test]
	fn gcd_calculation() {
		assert_eq!(
			gcd(BigSint::from(48u64), BigSint::from(18u64)),
			BigSint::from(6u64)
		);
		assert_eq!(
			gcd(BigSint::from(4800000u64), BigSint::from(1800000u64)),
			BigSint::from(600000u64)
		);
	}

	#[test]
	fn simplify() {
		let mut x =
			BigFraction::from_integers(BigSint::from(1111111110u64), BigSint::from(987654321u64));
		x.simplify_in_place();
		assert_eq!(x.numerator(), &BigSint::from(123456790u64));
		assert_eq!(x.denominator(), &BigSint::from(109739369u64));
	}
}
