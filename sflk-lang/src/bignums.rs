//! Cleaner version of the `bigint` module.
//!
//! TODO: Reimplement every feature of `bigint` but cleaner.
//! TODO: Comment the module.

mod big_unit {
	use std::{
		cmp::Ordering,
		ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign},
	};

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
	#[derive(Clone, Debug)]
	struct BigUint {
		/// The most significants digits are at the back.
		/// There shall not be insignificant leading zeros.
		/// The value zero is represented by an empty list of digits.
		///
		/// Respecting these rules ensures that each unsigned integer value can be
		/// represented by one unique representation.
		///
		/// For example, in base 10, the number 1234 would be stored as `vec![4, 3, 2, 1]`.
		digits: Vec<Digit>,
	}

	impl BigUint {
		fn zero() -> BigUint {
			BigUint { digits: Vec::new() }
		}

		/// Removes the insignificant leading zeros that may have been added
		/// by initialisation or for convinience.
		///
		/// A lot of methods expect the absence of insignificant leading zeros,
		/// and all the methods shall not allow `self` or a returned `BigUint`
		/// to contain insignificant leading zeros.
		fn remove_illegal_leading_zeros(&mut self) {
			while self.digits.last().copied() == Some(0) {
				self.digits.pop();
			}
		}

		/// Interpret the given list of digits as a sequence of digits that make up a number
		/// which is the value of the `BigUint` that is returned. The given digits are
		/// interpreted as the most significant digits being at the back
		/// (i.e. it is written "backwards", "from right to left").
		///
		/// This is faster than `from_digits_with_most_significant_at_the_beginning` as
		/// the most significant digits being at the back is already the layout of `BigUint`.
		///Mul
		/// For example, in base 10, giving `digits` = `vec![4, 3, 2, 1]` would construct
		/// a `BigUint` that represents the value 1234.
		/// Note: The base is not 10.
		fn from_digits_with_most_significant_at_the_back(mut digits: Vec<Digit>) -> BigUint {
			// The `digits` vector is already in the expected orientation for `BigUint`.
			let mut big_uint = BigUint { digits };

			big_uint.remove_illegal_leading_zeros();
			big_uint
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
						for digit in value.iter_digits_from_most_significant() {
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
		// TODO: Remove this seamingly useless method ?
		/// Get the `index`th most significant digit (i.e. iterating "from the left").
		///
		/// For example, in base 10, with `self` being the number 123456:
		/// - `index` = 0 would get `Some(1)`.
		/// - `index` = 2 would get `Some(3)`.
		/// - `index` = 9 would get `None`.
		fn get_nth_most_significant_digit(&self, index: usize) -> Option<Digit> {
			self.digits.iter().rev().copied().nth(index)
		}

		// TODO: Remove this seamingly useless method ?
		/// Get the `index`th least significant digit (i.e. iterating "from the right").
		///
		/// For example, in base 10, with `self` being the number 123456:
		/// - `index` = 0 would get `Some(6)`.
		/// - `index` = 2 would get `Some(4)`.
		/// - `index` = 9 would get `None`.
		fn get_nth_least_significant_digit(&self, index: usize) -> Option<Digit> {
			self.digits.iter().copied().nth(index)
		}

		// TODO: Remove this seamingly useless method ?
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
		fn iter_digits_from_most_significant(&self) -> impl Iterator<Item = Digit> + '_ {
			self.digits.iter().rev().copied()
		}

		/// Iterate over the digits, beginning with the least significant digits
		/// (i.e. iterating "from the right").
		///
		/// For example, in base 10, with `self` being the number 123456,
		/// the iterator would give (in order): 6, 5, 4, 3, 2, 1, and stop.
		fn iter_digits_from_least_significant(&self) -> impl Iterator<Item = Digit> + '_ {
			self.digits.iter().copied()
		}

		/// Iterate over the digits, beginning with the least significant digits
		/// (i.e. iterating "from the right")
		/// when considering that there are infinitely many insignificant leading zeros.
		///
		/// For example, in base 10, with `self` being the number 123456,
		/// the iterator would give (in order): 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, ... (never stops).
		fn iter_digits_from_least_significant_with_leading_zeros(
			&self,
		) -> impl Iterator<Item = Digit> + '_ {
			self.iter_digits_from_least_significant()
				.chain(std::iter::repeat(0))
		}
	}

	impl Eq for BigUint {}
	impl PartialEq for BigUint {
		fn eq(&self, rhs: &Self) -> bool {
			self.digits == rhs.digits
		}
	}

	impl Ord for BigUint {
		fn cmp(&self, rhs: &BigUint) -> Ordering {
			// First, look if one inetger has more digits than the `rhs`
			// in which case that would be the bigger one.
			if self.digits.len() < rhs.digits.len() {
				return Ordering::Less;
			} else if self.digits.len() > rhs.digits.len() {
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
			for (digit_self, digit_rhs) in self
				.iter_digits_from_most_significant()
				.zip(rhs.iter_digits_from_most_significant())
			{
				match digit_self.cmp(&digit_rhs) {
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
		fn partial_cmp(&self, rhs: &BigUint) -> Option<Ordering> {
			Some(self.cmp(rhs))
		}
	}

	impl AddAssign<&BigUint> for BigUint {
		fn add_assign(&mut self, rhs: &BigUint) {
			// When a digit to digit addition produces a result too big to fit
			// in the one digit being iterated over, this `carry` gets what
			// remains from the digit to digit sum to carry to the next iteration.
			let mut carry = 0;

			// Iterating beginning from the least significant digits.
			for i in 0.. {
				if i >= self.digits.len() && i >= rhs.digits.len() && carry == 0 {
					// There is no digit left nor a carry to add to `self`.
					break;
				}

				// Make sure there is a digit in `self` at index `i` to add something to.
				if i >= self.digits.len() {
					// ADDING LEADING ZERO
					// Unused leading zeros must be removed before returning.
					self.digits.push(0);
				}

				let self_digit = self.digits[i] as u64;
				let rhs_digit = rhs.get_nth_least_significant_digit_with_leading_zeros(i) as u64;

				// Perform one step of the addition, adding digit to digit and
				// handling the carry.
				let digit_sum = self_digit + rhs_digit + carry;
				self.digits[i] = (digit_sum % BASE) as Digit;
				let digit_sum_remians_devided_by_base = digit_sum / BASE;

				// The next iteration will be over one digit "to the left" (more significant),
				// thus this `carry` will be worth more (times `BASE` more) than during
				// this step, so is had to be divided by `BASE` to conserve value.
				carry = digit_sum_remians_devided_by_base;
				assert!(carry == 0 || carry == 1);
			}

			self.remove_illegal_leading_zeros();
		}
	}
	impl AddAssign<BigUint> for BigUint {
		fn add_assign(&mut self, rhs: BigUint) {
			*self += &rhs;
		}
	}
	impl Add<&BigUint> for BigUint {
		type Output = BigUint;
		fn add(mut self, rhs: &BigUint) -> BigUint {
			self += rhs;
			self
		}
	}
	impl Add<BigUint> for BigUint {
		type Output = BigUint;
		fn add(mut self, rhs: BigUint) -> BigUint {
			self += &rhs;
			self
		}
	}
	impl Add<&BigUint> for &BigUint {
		type Output = BigUint;
		fn add(self, rhs: &BigUint) -> BigUint {
			let mut res = self.clone();
			res += rhs;
			res
		}
	}
	impl Add<BigUint> for &BigUint {
		type Output = BigUint;
		fn add(self, mut rhs: BigUint) -> BigUint {
			// `+` is commutative.
			rhs += self;
			rhs
		}
	}

	impl SubAssign<&BigUint> for BigUint {
		fn sub_assign(&mut self, rhs: &BigUint) {
			// When a digit from `self` is too small to starnd the subtraction with
			// the digit from `rhs`, this `carry` helps by "moving" some value from
			// the digit from `self` that will be covered in the next iteration.
			let mut carry = 0;

			// Iterating beginning from the least significant digits.
			for i in 0.. {
				if i >= rhs.digits.len() && carry == 0 {
					// There is no digit left nor any carry to subtract from `self`.
					break;
				}

				if i >= self.digits.len() {
					// If we get to iterate past the most significant digit of `self`
					// while still having digitis or carries to subtract, then it means
					// that `self` was strictly bigger than `rhs`.
					panic!("subtracting a BigUint from a smaller BigUint");
				}

				let mut value_from_which_to_subtract = self.digits[i] as u64;
				let rhs_digit = rhs.get_nth_least_significant_digit_with_leading_zeros(i) as u64;

				// Perform one step of the subtraction, subtracting digit to digit and
				// handling the carry.
				let value_to_subtract = rhs_digit + carry;
				carry = 0;
				if value_from_which_to_subtract < value_to_subtract {
					// The digit from `self` to subtract to is not high enough,
					// so we "move" some value from the one digit of `self` "to the left"
					// (more significant) to add to the current one. The carry will carry
					// this information to the next iteration so that value is concerved.
					let value_moved_from_next_iteration = BASE;
					value_from_which_to_subtract += value_moved_from_next_iteration;
					// The next iteration will be over one digit "to the left" (more significant),
					// thus this `carry` will be worth more (times `BASE` more) than during
					// this step, so is has to be divided by `BASE` to conserve value.
					carry = value_moved_from_next_iteration / BASE;
				}
				assert!(carry == 0 || carry == 1);
				let digit_subtraction = value_from_which_to_subtract - value_to_subtract;

				assert!(
					digit_subtraction < BASE,
					"`digit_subtraction` is expected to fit in a digit"
				);
				self.digits[i] = digit_subtraction as Digit;
			}

			// Despite never explicitly adding insignificant leading zeros,
			// something like `x - x` would make all the digits to become zero,
			// and some other cases would make some of the leading digits to become zero,
			// so these potential leading zeros must be taken care of.
			self.remove_illegal_leading_zeros();
		}
	}
	impl SubAssign<BigUint> for BigUint {
		fn sub_assign(&mut self, rhs: BigUint) {
			*self -= &rhs;
		}
	}
	impl Sub<&BigUint> for BigUint {
		type Output = BigUint;
		fn sub(mut self, rhs: &BigUint) -> BigUint {
			self -= rhs;
			self
		}
	}
	impl Sub<BigUint> for BigUint {
		type Output = BigUint;
		fn sub(mut self, rhs: BigUint) -> BigUint {
			self -= &rhs;
			self
		}
	}
	impl Sub<&BigUint> for &BigUint {
		type Output = BigUint;
		fn sub(self, rhs: &BigUint) -> BigUint {
			let mut res = self.clone();
			res -= rhs;
			res
		}
	}
	impl Sub<BigUint> for &BigUint {
		type Output = BigUint;
		fn sub(self, rhs: BigUint) -> BigUint {
			let mut res = self.clone();
			res -= rhs;
			res
		}
	}

	impl Mul<&BigUint> for &BigUint {
		type Output = BigUint;

		fn mul(self, rhs: &BigUint) -> BigUint {
			// See [https://en.wikipedia.org/wiki/Multiplication_algorithm#Long_multiplication]
			// for more explanations on the algorithm used here.
			// It does not quite works in the same order we (at least in France) do it on paper
			// (doing number by digit multiplications, and then doing the sum of all the
			// intermediary results). Instead, it does digit by digit multiplications for all
			// the pairs of digits in the input, in an order that is similar to directly adding
			// the "current intermediary result (that is still not complete)" on the final result,
			// with a carry for the addition. Whatever, it works (and is more efficient than
			// would be storing all the intermediary results to add them later, or even just store
			// one intermediary result).

			let mut res_digits =
				Vec::from_iter(std::iter::repeat(0).take(self.digits.len() + rhs.digits.len()));

			for i_rhs in 0..rhs.digits.len() {
				let mut carry = 0;
				for i_self in 0..self.digits.len() {
					let self_digit = self.digits[i_self] as u64;
					let rhs_digit = rhs.digits[i_rhs] as u64;
					let i_res = i_self + i_rhs;
					let target_digit_before = res_digits[i_res] as u64;

					// Perform one step of the multiplication, multiplying digit to digit,
					// adding it to the result and handling the carry for the ongoing
					// addition.
					let input_digit_product = self_digit * rhs_digit;
					let digit_sum = target_digit_before + input_digit_product + carry;
					res_digits[i_res] = (digit_sum % BASE) as Digit;
					let digit_sum_remians_devided_by_base = digit_sum / BASE;

					// The next iteration will be over one digit "to the left" (more significant)
					// regarding the ongoing addition to the result,
					// thus this `carry` will be worth more (times `BASE` more) than during
					// this step, so is had to be divided by `BASE` to conserve value.
					carry = digit_sum_remians_devided_by_base;
				}
				// If any `carry` remains after all the digits to add, it must still be
				// counted in the result so as to not lose any value.
				// There is nothing in `res_digits` at this index yet, so there is
				// no need to add the `carry` to what was there before (which would be 0 anyway).
				res_digits[self.digits.len() + i_rhs] = carry as Digit;
			}

			// This conversion will take care of the potential remaining
			// insignificant leading zeros.
			BigUint::from_digits_with_most_significant_at_the_back(res_digits)
		}
	}
	impl Mul<BigUint> for &BigUint {
		type Output = BigUint;
		fn mul(self, rhs: BigUint) -> BigUint {
			let res = self * &rhs;
			res
		}
	}
	impl Mul<&BigUint> for BigUint {
		type Output = BigUint;
		fn mul(mut self, rhs: &BigUint) -> BigUint {
			self = &self * rhs;
			self
		}
	}
	impl Mul<BigUint> for BigUint {
		type Output = BigUint;
		fn mul(mut self, rhs: BigUint) -> BigUint {
			self = &self * &rhs;
			self
		}
	}
	impl MulAssign<BigUint> for BigUint {
		fn mul_assign(&mut self, rhs: BigUint) {
			*self = &*self * &rhs;
		}
	}
	impl MulAssign<&BigUint> for BigUint {
		fn mul_assign(&mut self, rhs: &BigUint) {
			*self = &*self * rhs;
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		impl BigUint {
			fn has_illegal_leading_zeros(&self) -> bool {
				self.digits.last().copied() == Some(0)
			}
		}

		/// A few integer values to iterate over in tests.
		fn some_values() -> impl Iterator<Item = u64> {
			let values: Vec<u64> = vec![
				0,
				1,
				8,
				17,
				42,
				69,
				100000000,
				123456789,
				0x123456789ABCDEF,
				BASE - 1,
				BASE,
				BASE + 1,
				u16::MAX as u64 - 123,
				u16::MAX as u64 - 1,
				u16::MAX as u64,
				u16::MAX as u64 + 1,
				u16::MAX as u64 + 123,
				u32::MAX as u64 - 123,
				u32::MAX as u64 - 1,
				u32::MAX as u64,
				u32::MAX as u64 + 1,
				u32::MAX as u64 + 123,
				u64::MAX - 123,
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
			let digits_bu: Vec<_> = bu.iter_digits_from_least_significant().collect();
			assert_eq!(
				digits_bu, digits_without_leading_zeros,
				"illegal leading zeros in BigUint when constructed from a digit vec"
			);
		}

		#[test]
		fn no_leading_zeros_from_primitive() {
			for value in some_values() {
				let bu = BigUint::from(value);
				assert!(
					!bu.has_illegal_leading_zeros(),
					"illegal leading zeros in BigUint when constructed from a primitive"
				);
			}
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
		fn too_big_to_fit() {
			let too_big_for_u32 = u32::MAX as u64 + 1;
			let does_not_fit = u32::try_from(&BigUint::from(too_big_for_u32));
			assert!(
				matches!(does_not_fit, Err(DoesNotFit)),
				"converting to u32 a BigUint that represents a value \
				too big to fit in a u32 must fail"
			);
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
						"(compare A to B) BigUint comparison behaves differently from Rust's"
					);
					assert_eq!(
						value_b.cmp(&value_a),
						bu_b.cmp(&bu_a),
						"(compare B to A) BigUint comparison behaves differently from Rust's"
					);
				}
			}
		}

		#[test]
		fn add() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					let bu_b = BigUint::from(value_b);
					let big_sum_a_b = {
						let mut tmp = bu_a.clone();
						tmp += &bu_b;
						tmp
					};
					assert!(
						!big_sum_a_b.has_illegal_leading_zeros(),
						"illegal leading zeros in BigUint resulting from addition"
					);

					// Check against Rust's result, if available.
					if let Some(sum_a_b) = value_a.checked_add(value_b) {
						let sum_a_b_after = u64::try_from(&big_sum_a_b).expect(
							"the checked addition passed, thus this was expected to pass too",
						);
						assert_eq!(
							sum_a_b, sum_a_b_after,
							"BigUint addition behaves differently from Rusts's"
						);
					}

					// Check consistency accross all the `impl` blocks related to the addition.
					assert_eq!(
						big_sum_a_b,
						{
							let mut tmp = bu_a.clone();
							tmp += bu_b.clone();
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						&bu_a + &bu_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						&bu_a + bu_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						bu_a.clone() + &bu_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						bu_a.clone() + bu_b.clone(),
						"one of the `impl`s is missbehaving"
					);
				}
			}
		}

		#[test]
		fn sub() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					// If Rust can't do the subtraction, neither can we.
					if let Some(subtraction_a_b) = value_a.checked_sub(value_b) {
						let bu_b = BigUint::from(value_b);
						let big_subtration_a_b = {
							let mut tmp = bu_a.clone();
							tmp -= &bu_b;
							tmp
						};
						assert!(
							!big_subtration_a_b.has_illegal_leading_zeros(),
							"illegal leading zeros in BigUint resulting from subtraction"
						);

						// Check against Rust's result.
						let subtraction_a_b_after = u64::try_from(&big_subtration_a_b).unwrap();
						assert_eq!(
							subtraction_a_b, subtraction_a_b_after,
							"BigUint subctarction behaves differently from Rusts's"
						);

						// Check consistency accross all the `impl` blocks related
						// to the subtraction.
						assert_eq!(
							big_subtration_a_b,
							{
								let mut tmp = bu_a.clone();
								tmp -= bu_b.clone();
								tmp
							},
							"one of the `impl`s is missbehaving"
						);
						assert_eq!(
							big_subtration_a_b,
							&bu_a - &bu_b,
							"one of the `impl`s is missbehaving"
						);
						assert_eq!(
							big_subtration_a_b,
							&bu_a - bu_b.clone(),
							"one of the `impl`s is missbehaving"
						);
						assert_eq!(
							big_subtration_a_b,
							bu_a.clone() - &bu_b,
							"one of the `impl`s is missbehaving"
						);
						assert_eq!(
							big_subtration_a_b,
							bu_a.clone() - bu_b.clone(),
							"one of the `impl`s is missbehaving"
						);
					}
				}
			}
		}

		#[test]
		#[should_panic]
		fn sub_small_minus_big() {
			// 8 - 42 = -34 which require a sign (not available with `BigUint`),
			// so this should not work and is expected to panic.
			let _does_not_work = BigUint::from(8u64) - BigUint::from(42u64);
		}

		#[test]
		fn mul() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					let bu_b = BigUint::from(value_b);
					let big_product_a_b = &bu_a * &bu_b;
					assert!(
						!big_product_a_b.has_illegal_leading_zeros(),
						"illegal leading zeros in BigUint resulting from multiplication"
					);

					// Check against Rust's result, if available.
					if let Some(product_a_b) = value_a.checked_mul(value_b) {
						let product_a_b_after = u64::try_from(&big_product_a_b).expect(
							"the checked multiplication passed, \
							thus this was expected to pass too",
						);
						assert_eq!(
							product_a_b, product_a_b_after,
							"BigUint multiplication behaves differently from Rusts's"
						);
					}

					// Check consistency accross all the `impl` blocks related
					// to the multiplication.
					assert_eq!(
						big_product_a_b,
						&bu_a * bu_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						bu_a.clone() * &bu_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						bu_a.clone() * bu_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						{
							let mut tmp = bu_a.clone();
							tmp *= &bu_b;
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						{
							let mut tmp = bu_a.clone();
							tmp *= bu_b.clone();
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
				}
			}
		}
	}
}
