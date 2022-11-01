//! Cleaner version of the `bigint` module.
//!
//! TODO: Reimplement every feature of `bigint` but cleaner.
//! TODO: Comment the module.

mod big_unit {
	use std::cmp::Ordering;

	/// The type of one digit. The base used for the representation of the digits is `BASE`,
	/// which should be the number of values that the `Digit` type can represent.
	///
	/// It does not have to be `u8` (although some tests may rely on that),
	/// this is an arbitrary decision, and an uneducated guess at that.
	///
	/// TODO: Benchmark heavy math hapenning with different types for `Digit`.
	/// I heard Python uses digits that fit closely in 32-bits or something,
	/// maybe a `u32` could be more appropriate.
	type Digit = u8;

	/// The base should be of type `u64` (as a lot of computations are done with `u64`s)
	/// and is the number of values that the `Digit` type can represent.
	const BASE: u64 = Digit::MAX as u64 + 1;

	/// Unsigned big integer. Actually a list of digits in base `BASE`
	/// represented with the integer type `Digit`.
	#[derive(Debug)]
	struct BigUint {
		/// The most significants digits are at the back.
		/// There shall not be leading zeros.
		/// The value zero is represented by an empty list of digits.
		///
		/// Respecting these rules ensures that each unsigned integer value can be
		/// represented by one unique representation.
		///
		/// For example, in base 10, the number 1234 would be stored as `vec![4, 3, 2, 1]`.
		digits: Vec<Digit>,
	}

	impl BigUint {
		/// Interpret the given list of digits as a sequence of digits that make up a number
		/// which is the value of the `BigUint` that is returned. The given digits are
		/// interpreted as the most significant digits being at the back
		/// (i.e. it is written "backwards", "from right to left").
		///
		/// This is faster than `from_digits_with_most_significant_at_the_beginning` as
		/// the most significant digits being at the back is already the layout of `BigUint`.
		///
		/// For example, in base 10, giving `digits` = `vec![4, 3, 2, 1]` would construct
		/// a `BigUint` that represents the value 1234.
		/// Note: The base is not 10.
		fn from_digits_with_most_significant_at_the_back(mut digits: Vec<Digit>) -> BigUint {
			// The `digits` vector is already in the expected orientation for `BigUint`.

			// Removes leading zeros.
			while digits.last() == Some(&0) {
				digits.pop();
			}

			BigUint { digits }
		}

		/// Interpret the given list of digits as a sequence of digits that make up a number
		/// which is the value of the `BigUint` that is returned. The given digits are
		/// interpreted as the least significant digits being at the back
		/// (i.e. it is written "normally", "forward", "from left to right").
		///
		/// This is slower than `from_digits_with_most_significant_at_the_back`.
		///
		/// For example, in base 10, giving `digits` = `vec![1, 2, 3, 4]` would construct
		/// a `BigUint` that represents the value 1234.
		/// Note: The base is not 10.
		fn from_digits_with_most_significant_at_the_beginning(mut digits: Vec<Digit>) -> BigUint {
			// The `digits` vector is NOT in the expected orientation for `BigUint`,
			// it must be reversed.
			digits.reverse();
			BigUint::from_digits_with_most_significant_at_the_back(digits)
		}
	}

	/// Locat trait used to tag a few primitive integer types that can be converted
	/// to a `BigUint`. Bounding to this local trait is allowed by the orphan rule,
	/// unlike bounding directly to `Into<u64>`.
	trait LocalIntoU64: Into<u64> {}
	macro_rules! impl_local_into_u64 {
		($($primitive_type:ty),*) => {
			$(
				impl LocalIntoU64 for $primitive_type {}
			)*
		}
	}
	impl_local_into_u64!(u8, u16, u32, u64);

	impl<T: LocalIntoU64> From<T> for BigUint {
		fn from(value: T) -> BigUint {
			let mut value: u64 = value.into();
			let mut digits: Vec<Digit> = Vec::new();

			// Extract the digits in base `BASE` from `value`,
			// with the most significants digits at the back
			// (same layout in BigUint so faster conversion).
			while value > 0 {
				digits.push((u64::from(value) % BASE) as Digit);
				value = value as u64 / BASE;
			}

			BigUint::from_digits_with_most_significant_at_the_back(digits)
		}
	}

	/// A conversion from a big number into a primitive integer type have failed due
	/// to the value being too big to be representable by the primitive integer type.
	#[derive(Debug)]
	pub struct DoesNotFit;

	/// Both the primpitive types and the `TryFrom` trait are not local to this crate,
	/// thus the orphan rule forbids a nice `impl` block that generalizes all the convenrsions,
	/// and a local trait to tag the primitive types won't work here somehow.
	/// However, one by one, it works. Well then, how about one by one but all at once.
	macro_rules! impl_try_from_big_uint {
		($($primitive_type:ty),*) => {
			$(
				impl TryFrom<&BigUint> for $primitive_type {
					type Error = DoesNotFit;

					fn try_from(value: &BigUint) -> Result<$primitive_type, DoesNotFit> {
						let mut acc = 0 as $primitive_type;
						for &digit in value.digits.iter().rev() {
							acc = acc.checked_mul(BASE as $primitive_type).ok_or(DoesNotFit)?;
							acc += digit as $primitive_type;
						}
						Ok(acc)
					}
				}
			)*
		}
	}
	impl_try_from_big_uint!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

	impl BigUint {
		/// Get the `index`th most significant digit (i.e. iterating "from the left").
		///
		/// For example, in base 10, with `self` being the number 123456:
		/// - `index` = 0 would get `Some(1)`.
		/// - `index` = 2 would get `Some(3)`.
		/// - `index` = 9 would get `None`.
		fn get_nth_most_significant_digit(&self, index: usize) -> Option<Digit> {
			self.digits.iter().rev().copied().nth(index)
		}

		/// Get the `index`th least significant digit (i.e. iterating "from the right").
		///
		/// For example, in base 10, with `self` being the number 123456:
		/// - `index` = 0 would get `Some(6)`.
		/// - `index` = 2 would get `Some(4)`.
		/// - `index` = 9 would get `None`.
		fn get_nth_least_significant_digit(&self, index: usize) -> Option<Digit> {
			self.digits.iter().copied().nth(index)
		}

		/// Get the `index`th least significant digit (i.e. iterating "from the right")
		/// when considering that there are infinitely many insignificant leading zeros.
		///
		/// For example, in base 10, with `self` being the number 123456:
		/// - `index` = 0 would get 6.
		/// - `index` = 2 would get 4.
		/// - `index` = 9 would get 0 (a virtual leading zero in ...00000123456).
		fn get_nth_least_significant_digit_with_leading_zeros(&self, index: usize) -> Digit {
			self.get_nth_least_significant_digit(index).unwrap_or(0)
		}

		/// Iterate over the digits, beginning with the most significant digits
		/// (i.e. iterating "from the left").
		///
		/// For example, in base 10, with `self` being the number 123456,
		/// the iterator would give (in order): 1, 2, 3, 4, 5, 6, and stop.
		fn digits_from_most_significant(&self) -> impl Iterator<Item = Digit> + '_ {
			self.digits.iter().rev().copied()
		}

		/// Iterate over the digits, beginning with the least significant digits
		/// (i.e. iterating "from the right").
		///
		/// For example, in base 10, with `self` being the number 123456,
		/// the iterator would give (in order): 6, 5, 4, 3, 2, 1, and stop.
		fn digits_from_least_significant(&self) -> impl Iterator<Item = Digit> + '_ {
			self.digits.iter().copied()
		}

		/// Iterate over the digits, beginning with the least significant digits
		/// (i.e. iterating "from the right")
		/// when considering that there are infinitely many insignificant leading zeros.
		///
		/// For example, in base 10, with `self` being the number 123456,
		/// the iterator would give (in order): 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, ... (never stops).
		fn digits_from_least_significant_with_leading_zeros(
			&self,
		) -> impl Iterator<Item = Digit> + '_ {
			self.digits_from_least_significant()
				.chain(std::iter::repeat(0))
		}
	}

	impl Eq for BigUint {}
	impl PartialEq for BigUint {
		fn eq(&self, other: &Self) -> bool {
			self.digits == other.digits
		}
	}

	impl Ord for BigUint {
		fn cmp(&self, other: &BigUint) -> Ordering {
			// First, look if one inetger has more digits than the other
			// in which case that would be the bigger one.
			if self.digits.len() < other.digits.len() {
				return Ordering::Less;
			} else if self.digits.len() > other.digits.len() {
				return Ordering::Greater;
			}

			// Both integers have the same number of digits.
			// Now, compare the digits, one to one.
			// `zip` works since both iterators will stop at the same time.
			//
			// We start from the most significant digits because the only digit to digit
			// difference that matters is the most significant one.
			//
			// For example, in the comparison `11811211 > 11611911`, the only
			// digit to digit comparison that matters is 8 > 6 that that is because
			// this is the most significant digit to digit comparison.
			for (digit_self, digit_other) in self
				.digits_from_most_significant()
				.zip(other.digits_from_most_significant())
			{
				match digit_self.cmp(&digit_other) {
					Ordering::Greater => return Ordering::Greater,
					Ordering::Less => return Ordering::Less,
					Ordering::Equal => (),
				}
			}

			// There is no difference in the digits of the numbers.
			Ordering::Equal
		}
	}
	impl PartialOrd for BigUint {
		fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
			Some(self.cmp(other))
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		/// A few integer values (in increasing order) to iterate over in tests.
		fn some_values() -> impl Iterator<Item = u64> {
			assert!(69 < BASE - 1, "`values` should be strictly increasing");
			assert!(
				BASE + 1 < 123456789,
				"`values` should be strictly increasing"
			);
			let values: Vec<u64> = vec![
				0,
				69,
				BASE - 1,
				BASE,
				BASE + 1,
				123456789,
				u64::MAX - 1,
				u64::MAX,
			];
			values.into_iter()
		}

		#[test]
		fn no_leading_zeros_from_digits() {
			let digits_without_leading_zeros: Vec<Digit> = vec![0x69, 0x42, 0xCA, 0xCA];
			let digits_with_leading_zeros: Vec<Digit> = {
				let mut vec = digits_without_leading_zeros.clone();
				vec.append(&mut vec![0x00, 0x00, 0x00]);
				vec
			};
			let bu =
				BigUint::from_digits_with_most_significant_at_the_back(digits_with_leading_zeros);
			let digits_bu: Vec<_> = bu.digits_from_least_significant().collect();
			assert_eq!(
				digits_bu, digits_without_leading_zeros,
				"leading zeros in BigUint when constructed from a digit vec"
			);
		}

		#[test]
		fn no_leading_zeros_from_primitive() {
			// Such a value is small, if converted naively it may have leading zeros.
			let small_vlaue_in_large_primitive = 69u64;
			let bu = BigUint::from(small_vlaue_in_large_primitive);
			assert!(
				bu.digits.first() != Some(&0),
				"leading zeros in BigUint when constructed from a primitive"
			);
		}

		#[test]
		fn preserve_value() {
			for value in some_values() {
				let value_before = value;
				let bu = BigUint::from(value_before);
				let value_after = u64::try_from(&bu).unwrap();
				assert_eq!(
					value_before, value_after,
					"converting to and then from a BigUint does not preserve the value"
				);
			}
		}

		#[test]
		#[should_panic]
		fn too_big_to_fit() {
			let too_big_for_u32 = u32::MAX as u64 + 1;
			let _too_small: u32 = (&BigUint::from(too_big_for_u32)).try_into().unwrap();
		}

		#[test]
		fn eq_with_itself() {
			for value in some_values() {
				let bu = BigUint::from(value);
				assert_eq!(bu, bu, "BigUint is not equal with itself");
			}
		}

		#[test]
		fn ord() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					let bu_b = BigUint::from(value_b);
					assert_eq!(
						value_a.cmp(&value_b),
						bu_a.cmp(&bu_b),
						"BigUint comparison different from Rust's"
					);
				}
			}
		}
	}
}
