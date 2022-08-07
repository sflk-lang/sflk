type Digit = u8;

const BASE: u64 = 256;

/// An unsigned big integer.
/// The base is `BASE`, the most significants digits are at the beginning.
struct BigUint {
	digits: Vec<Digit>,
}

impl BigUint {
	fn zero() -> BigUint {
		BigUint { digits: Vec::new() }
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
	fn u64_to_BigUint_to_u64() {
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
			let mut big_a = BigUint::from_u64(a);
			let big_b = BigUint::from_u64(b);
			big_a.add_in_place(&big_b);
			let big_sum = big_a;
			assert_eq!(big_sum.to_u64(), a + b);
		}
	}
}
