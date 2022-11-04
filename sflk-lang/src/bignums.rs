//! Cleaner version of the `bigint` module.
//!
//! TODO: Reimplement every feature of `bigint` but cleaner.
//! TODO: Comment the module.

/// A conversion from a big number into a primitive integer type have failed due
/// to the value being ouside the representable range of values supported by the
/// primitive integer type.
#[derive(Debug)]
pub struct DoesNotFitInPrimitive;

mod big_unit {
	use super::DoesNotFitInPrimitive;
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
	///
	/// TODO: Make `BigUint` be generic over the type used for digitis, and over the `BASE` value.
	type Digit = u8;

	/// The base should be of type `u64` (as a lot of computations are done with `u64`s)
	/// and is the number of values that the `Digit` type can represent.
	const BASE: u64 = Digit::MAX as u64 + 1;

	/// Unsigned big integer. Actually a list of digits in base `BASE`
	/// represented with the integer type `Digit`.
	#[derive(Clone, Debug)]
	pub struct BigUint {
		/// The most significants digits are at the back.
		/// There shall not be insignificant leading zeros.
		/// The value zero is represented by an empty list of digits.
		///
		/// Respecting these rules ensures that each unsigned integer value can be
		/// represented by one unique representation, code is simpler in some places.
		///
		/// For example, in base 10, the number 1234 would be stored as `vec![4, 3, 2, 1]`.
		digits: Vec<Digit>,
	}

	impl BigUint {
		pub fn zero() -> BigUint {
			BigUint { digits: Vec::new() }
		}

		pub fn is_zero(&self) -> bool {
			self.digits.is_empty()
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
		/// For example, in base 10, giving `digits` = `vec![4, 3, 2, 1]` would construct
		/// a `BigUint` that represents the value 1234.
		/// Note: The base is not 10.
		fn from_digits_with_most_significant_at_the_back(digits: Vec<Digit>) -> BigUint {
			// The `digits` vector is already in the expected orientation for `BigUint`.
			let mut big_uint = BigUint { digits };

			big_uint.remove_illegal_leading_zeros();
			big_uint
		}

		/// Iterate over the digits, beginning with the most significant digits
		/// (i.e. iterating "from the left").
		///
		/// For example, in base 10, with `self` being the number 123456,
		/// the iterator would give (in order): 1, 2, 3, 4, 5, 6, and stop.
		fn iter_digits_from_most_significant(&self) -> impl Iterator<Item = Digit> + '_ {
			self.digits.iter().rev().copied()
		}
	}

	/// Locat trait used to tag a few primitive integer types that can be converted
	/// to `BigUint`. Bounding to this local trait is allowed by the orphan rule,
	/// unlike bounding directly to `Into<u64>`.
	pub trait LocalIntoU64: Into<u64> {}
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

	/// Both the primpitive types and the `TryFrom` trait are not local to this crate,
	/// thus the orphan rule forbids a nice `impl` block that generalizes all the convenrsions,
	/// and a local trait to tag the primitive types won't work here somehow.
	/// However, one by one, it works. Well then, how about one by one but all at once.
	macro_rules! impl_try_from_big_uint {
		($($primitive_type:ty),*) => {
			$(
				impl TryFrom<&BigUint> for $primitive_type {
					type Error = DoesNotFitInPrimitive;

					fn try_from(
						value: &BigUint
					) -> Result<$primitive_type, DoesNotFitInPrimitive> {
						let mut acc = 0 as $primitive_type;
						for digit in value.iter_digits_from_most_significant() {
							acc = acc.checked_mul(BASE as $primitive_type)
								.ok_or(DoesNotFitInPrimitive)?;
							acc += digit as $primitive_type;
						}
						Ok(acc)
					}
				}
			)*
		}
	}
	impl_try_from_big_uint!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

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
				let rhs_digit = rhs.digits.get(i).copied().unwrap_or(0) as u64;

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
				let rhs_digit = rhs.digits.get(i).copied().unwrap_or(0) as u64;

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
			//
			// Note: Doing a multiplication on paper might help to understand.

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

	impl BigUint {
		/// Performs the euclidian division `self / rhs`,
		/// returns `(quotient, remainder)`.
		#[must_use]
		pub fn div_euclidian(&self, rhs: &BigUint) -> (BigUint, BigUint) {
			// Classic long division algorithm.
			// Surprisingly, it is pretty hard to find a readable and complete implementation
			// or pseudocode of this algorithm on the Internet (or I haven't searched good enough),
			// so this is a custom version. It works (for what it's worth).
			//
			// Note: Doing a division on paper will probably help to understand.

			assert_ne!(rhs, &BigUint::zero(), "dividing a BigUint by zero");

			let mut quotient_digits = Vec::from_iter(std::iter::repeat(0).take(self.digits.len()));
			let mut current = BigUint::zero();

			for i in 1..=self.digits.len() {
				// Append "to the right" of the result of the last subtraction
				// (or to 0 for the first iteration) the next most significant digit of `self`
				// that have not yet been considered.
				current.digits.insert(0, self.digits[self.digits.len() - i]);
				// Might be an insignificant leading zero that causes problems
				// in a subtraction later.
				current.remove_illegal_leading_zeros();

				// Now must happen a subtraction between `current` (the bigger number
				// in the subtraction) and the biggets possible multiple of `rhs`.
				// To find this biggest multiple (let it be `factor * rhs`),
				// `factor` is searched for (in a fast way), then the subtraction
				// happens and the result becomes the next `current`.
				// `current - factor * rhs = next_current`.
				// `factor` is supposed to be smaller than `BASE` and is the next
				// digit to be appended "to the right" of the `quotient`.
				let factor = if &current < rhs {
					// The case when `factor == 0` seem to be a special case for the
					// following algorithm that searches `factor`.
					0
				} else {
					// That bigest multiple of `rhs` that is strictly smaller than `current`
					// is searched for in the "same order" that a binary search is done
					// (see [https://en.wikipedia.org/wiki/Binary_search_algorithm] or something).
					//
					// `factor` is supposed to be between 0 and `BASE` (excluded), so only this
					// range is scanned. A failed attemp can tell us if ths `factor` we search is
					// smaller or bigger, so we can cut in half the range to scan each try.
					//
					// For example, if `BASE` is 256 and the `factor` we search is 100, the values
					// that will be tested are (in order):
					//         tested `factor`: 128, 64, 96, 112, 104, 100
					//   `BASE / power_of_two`: 64,  32, 16,   8,   4,   2 ...
					let mut power_of_two = 2;
					let mut factor = BASE / power_of_two;
					let mut factor_times_rhs = BigUint::from(factor) * rhs;
					loop {
						power_of_two *= 2;
						if current >= factor_times_rhs {
							// We have `0 <= current - factor * rhs`, now we check if
							// this `factor` is the biggest possible that has this property.
							let subtraction_result = &current - factor_times_rhs;
							if &subtraction_result < rhs {
								// This is the `factor` we search (as we have
								// `0 <= current - factor * rhs < rhs`, both increasing or
								// decreasing `factor` would make this invalid)!
								// Also, the subtrcation result is the next `current`.
								current = subtraction_result;
								break factor;
							} else {
								// The `factor` we search is bigger.
								factor += BASE / power_of_two;
							}
						} else {
							// The `factor` we search is smaller
							// (since `current < factor * rhs` and we want
							// `0 <= current - factor * rhs`).
							factor -= BASE / power_of_two;
						}
						factor_times_rhs = BigUint::from(factor) * rhs;
					}
				};

				// Append `factor` as a digit "to the right" of the `quotient`
				// (its new least significant digit).
				assert!(factor < BASE);
				let i_quotient = quotient_digits.len() - i;
				quotient_digits[i_quotient] = factor as Digit;
			}

			let remainder = current;
			// This conversion will take care of the potential remaining
			// insignificant leading zeros.
			let quotient = BigUint::from_digits_with_most_significant_at_the_back(quotient_digits);
			(quotient, remainder)
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
			assert_eq!(
				bu.digits, digits_without_leading_zeros,
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
				matches!(does_not_fit, Err(DoesNotFitInPrimitive)),
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
		fn eq() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					let bu_b = BigUint::from(value_b);
					assert_eq!(
						value_a.eq(&value_b),
						bu_a.eq(&bu_b),
						"BigUint equality test behaves differently from Rust's"
					);
				}
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
						"BigUint comparison behaves differently from Rust's"
					);
				}
			}
		}

		#[test]
		fn eq_consistent_with_ord() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					let bu_b = BigUint::from(value_b);
					assert_eq!(
						bu_a.eq(&bu_b),
						bu_a.cmp(&bu_b) == Ordering::Equal,
						"BigUint comparison (cmp) behaves differently BigUint equality test (eq)"
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

		#[test]
		fn div_euclidian() {
			for value_a in some_values() {
				let bu_a = BigUint::from(value_a);
				for value_b in some_values() {
					// If Rust can't do the division (because `value_b` is zero), neither can we.
					if let Some(quotient_a_b) = value_a.checked_div(value_b) {
						let remainder_a_b = value_a % value_b;

						let bu_b = BigUint::from(value_b);
						let (big_quotient_a_b, big_remainder_a_b) = bu_a.div_euclidian(&bu_b);
						assert!(
							!big_quotient_a_b.has_illegal_leading_zeros(),
							"illegal leading zeros in BigUint resulting from \
							euclidian division (quotient)"
						);
						assert!(
							!big_remainder_a_b.has_illegal_leading_zeros(),
							"illegal leading zeros in BigUint resulting from \
							euclidian division (remainder)"
						);

						// Check against Rust's result.
						let quotient_a_b_after = u64::try_from(&big_quotient_a_b).unwrap();
						assert_eq!(
							quotient_a_b, quotient_a_b_after,
							"BigUint euclidian behaves differently from Rusts's for the quotient"
						);
						let remainder_a_b_after = u64::try_from(&big_remainder_a_b).unwrap();
						assert_eq!(
							remainder_a_b, remainder_a_b_after,
							"BigUint euclidian behaves differently from Rusts's for the remainder"
						);
					}
				}
			}
		}

		#[test]
		#[should_panic]
		fn div_by_zero() {
			let _does_not_work = BigUint::from(69u64).div_euclidian(&BigUint::zero());
		}
	}
}

mod big_sint {
	use std::{
		cmp::Ordering,
		ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign},
	};

	use super::{big_unit::BigUint, DoesNotFitInPrimitive};

	/// Big signed integer.
	#[derive(Clone, Debug)]
	pub struct BigSint {
		abs_value: BigUint,
		/// There is no constraint on this when the value is zero.
		is_negative: bool,
	}

	impl BigSint {
		pub fn zero() -> BigSint {
			BigSint { abs_value: BigUint::zero(), is_negative: false }
		}

		pub fn is_zero(&self) -> bool {
			self.abs_value.is_zero()
		}

		pub fn is_positive(&self) -> bool {
			(!self.is_negative) || self.is_zero()
		}

		pub fn is_strictly_positive(&self) -> bool {
			(!self.is_negative) && (!self.is_zero())
		}

		pub fn is_negative(&self) -> bool {
			self.is_negative || self.is_zero()
		}

		pub fn is_strictly_negative(&self) -> bool {
			self.is_negative && (!self.is_zero())
		}

		pub fn negate(&mut self) {
			self.is_negative = !self.is_negative;
		}

		pub fn abs(self) -> BigSint {
			BigSint { abs_value: self.abs_value, is_negative: false }
		}
	}

	impl From<BigUint> for BigSint {
		fn from(value: BigUint) -> BigSint {
			BigSint { abs_value: value, is_negative: false }
		}
	}

	impl<T: super::big_unit::LocalIntoU64> From<T> for BigSint {
		fn from(value: T) -> BigSint {
			let value: u64 = value.into();
			BigSint {
				abs_value: BigUint::from(value),
				is_negative: false,
			}
		}
	}

	/// Rust forbids an other `impl From<T> for BigSint` block even if T is bound
	/// to a trait that does not overlap with the trait bound of the previous block.
	/// This is because an hypothetical programmer could potentially sneak in and
	/// add `impl`s that make the two non-overlapping traits now overlapping.
	macro_rules! impl_from_into_i64 {
		($($primitive_type:ty),*) => {
			$(
				impl From<$primitive_type> for BigSint {
					fn from(value: $primitive_type) -> BigSint {
						let value: i64 = value.into();
						BigSint {
							abs_value: BigUint::from(value.abs() as u64),
							is_negative: value < 0,
						}
					}
				}
			)*
		}
	}
	impl_from_into_i64!(i8, i16, i32, i64);

	macro_rules! impl_try_from_big_sint_unsigned {
		($($primitive_type:ty),*) => {
			$(
				impl TryFrom<&BigSint> for $primitive_type {
					type Error = DoesNotFitInPrimitive;

					fn try_from(
						value: &BigSint
					) -> Result<$primitive_type, DoesNotFitInPrimitive> {
						if value.is_negative {
							Err(DoesNotFitInPrimitive)
						} else {
							Ok((&value.abs_value).try_into()?)
						}
					}
				}
			)*
		}
	}

	impl_try_from_big_sint_unsigned!(u8, u16, u32, u64, usize);

	macro_rules! impl_try_from_big_sint_signed {
		($(($primitive_type:ty, $unsigned_intermediary_type:ty)),*) => {
			$(
				impl TryFrom<&BigSint> for $primitive_type {
					type Error = DoesNotFitInPrimitive;

					fn try_from(
						value: &BigSint
					) -> Result<$primitive_type, DoesNotFitInPrimitive> {
						// The given `$unsigned_intermediary_type` is supposed to be able to hold
						// the absolute value (even for `<$primitive_type>::MIN`).
						let intermediary: $unsigned_intermediary_type =
							(&value.abs_value).try_into()?;
						if (
							intermediary ==
								<$primitive_type>::MAX as $unsigned_intermediary_type + 1
							&& value.is_negative
						) {
							// The case of the value being `<$primitive_type>::MIN` cannot
							// be handled naively as its abolute value does not fit in
							// a value of type `$primitive_type`.
							Ok(<$primitive_type>::MIN)
						} else {
							let res: $primitive_type =
								intermediary.try_into().map_err(|_| DoesNotFitInPrimitive)?;
							let sign: $primitive_type =
								if value.is_negative { -1 as $primitive_type } else { 1 };
							Ok(res * sign)
						}
					}
				}
			)*
		}
	}
	impl_try_from_big_sint_signed!(
		(i8, u16),
		(i16, u32),
		(i32, u64),
		(i64, u128),
		(isize, u128)
	);

	impl Eq for BigSint {}
	impl PartialEq for BigSint {
		fn eq(&self, rhs: &BigSint) -> bool {
			(self.is_zero() && rhs.is_zero())
				|| (self.abs_value == rhs.abs_value && self.is_negative == rhs.is_negative)
		}
	}

	impl Ord for BigSint {
		fn cmp(&self, rhs: &BigSint) -> Ordering {
			if self.abs_value.is_zero() && rhs.abs_value.is_zero() {
				Ordering::Equal
			} else {
				match (self.is_negative, rhs.is_negative) {
					(false, false) => self.abs_value.cmp(&rhs.abs_value),
					(true, true) => rhs.abs_value.cmp(&self.abs_value),
					(true, false) => Ordering::Less,
					(false, true) => Ordering::Greater,
				}
			}
		}
	}
	impl PartialOrd for BigSint {
		fn partial_cmp(&self, rhs: &BigSint) -> Option<Ordering> {
			Some(self.cmp(rhs))
		}
	}

	impl BigSint {
		fn add_assign_ex(&mut self, rhs: &BigSint, flip_sign_of_rhs: bool) {
			let rhs_is_negative = if flip_sign_of_rhs {
				!rhs.is_negative
			} else {
				rhs.is_negative
			};
			// TODO: Try to remove useless clones in some subtractions without
			// duplicating too much code.
			match (self.is_negative, rhs_is_negative) {
				(false, false) | (true, true) => {
					// (+1) + (+2) = +3 = +(1 + 2)
					// (-1) + (-2) = -3 = -(1 + 2)
					self.abs_value += &rhs.abs_value;
				},
				(false, true) if self.abs_value >= rhs.abs_value => {
					// (+8) + (-3) = +5 = +(8 - 3)
					self.abs_value -= &rhs.abs_value;
					self.is_negative = false;
				},
				(false, true) => {
					// (+3) + (-8) = -5 = -(8 - 3)
					// TODO: Remove useless clone somehow.
					self.abs_value = &rhs.abs_value - &self.abs_value;
					self.is_negative = true;
				},
				(true, false) if self.abs_value > rhs.abs_value => {
					// (-8) + (+3) = -5 = -(8 - 3)
					self.abs_value -= &rhs.abs_value;
					self.is_negative = true;
				},
				(true, false) => {
					// (-3) + (+8) = +5 = +(8 - 3)
					// TODO: Remove useless clone somehow.
					self.abs_value = &rhs.abs_value - &self.abs_value;
					self.is_negative = false;
				},
			}
		}
		fn add_ex(&self, rhs: &BigSint, flip_sign_of_rhs: bool) -> BigSint {
			let mut res = self.clone();
			res.add_assign_ex(rhs, flip_sign_of_rhs);
			res
		}
	}

	impl AddAssign<BigSint> for BigSint {
		fn add_assign(&mut self, rhs: BigSint) {
			self.add_assign_ex(&rhs, false);
		}
	}
	impl AddAssign<&BigSint> for BigSint {
		fn add_assign(&mut self, rhs: &BigSint) {
			self.add_assign_ex(rhs, false);
		}
	}
	impl Add<BigSint> for BigSint {
		type Output = BigSint;
		fn add(self, rhs: BigSint) -> BigSint {
			self.add_ex(&rhs, false)
		}
	}
	impl Add<&BigSint> for BigSint {
		type Output = BigSint;
		fn add(self, rhs: &BigSint) -> BigSint {
			self.add_ex(rhs, false)
		}
	}
	impl Add<BigSint> for &BigSint {
		type Output = BigSint;
		fn add(self, rhs: BigSint) -> BigSint {
			self.add_ex(&rhs, false)
		}
	}
	impl Add<&BigSint> for &BigSint {
		type Output = BigSint;
		fn add(self, rhs: &BigSint) -> BigSint {
			self.add_ex(rhs, false)
		}
	}

	impl SubAssign<BigSint> for BigSint {
		fn sub_assign(&mut self, rhs: BigSint) {
			self.add_assign_ex(&rhs, true);
		}
	}
	impl SubAssign<&BigSint> for BigSint {
		fn sub_assign(&mut self, rhs: &BigSint) {
			self.add_assign_ex(rhs, true);
		}
	}
	impl Sub<BigSint> for BigSint {
		type Output = BigSint;
		fn sub(self, rhs: BigSint) -> BigSint {
			self.add_ex(&rhs, true)
		}
	}
	impl Sub<&BigSint> for BigSint {
		type Output = BigSint;
		fn sub(self, rhs: &BigSint) -> BigSint {
			self.add_ex(rhs, true)
		}
	}
	impl Sub<BigSint> for &BigSint {
		type Output = BigSint;
		fn sub(self, rhs: BigSint) -> BigSint {
			self.add_ex(&rhs, true)
		}
	}
	impl Sub<&BigSint> for &BigSint {
		type Output = BigSint;
		fn sub(self, rhs: &BigSint) -> BigSint {
			self.add_ex(rhs, true)
		}
	}

	impl MulAssign<BigSint> for BigSint {
		fn mul_assign(&mut self, rhs: BigSint) {
			self.is_negative ^= rhs.is_negative;
			self.abs_value *= rhs.abs_value;
		}
	}
	impl MulAssign<&BigSint> for BigSint {
		fn mul_assign(&mut self, rhs: &BigSint) {
			self.is_negative ^= rhs.is_negative;
			self.abs_value *= &rhs.abs_value;
		}
	}
	impl Mul<BigSint> for BigSint {
		type Output = BigSint;
		fn mul(mut self, rhs: BigSint) -> BigSint {
			self *= rhs;
			self
		}
	}
	impl Mul<&BigSint> for BigSint {
		type Output = BigSint;
		fn mul(mut self, rhs: &BigSint) -> BigSint {
			self *= rhs;
			self
		}
	}
	impl Mul<BigSint> for &BigSint {
		type Output = BigSint;
		fn mul(self, mut rhs: BigSint) -> BigSint {
			// `*` is commutative.
			rhs *= self;
			rhs
		}
	}
	impl Mul<&BigSint> for &BigSint {
		type Output = BigSint;
		fn mul(self, rhs: &BigSint) -> BigSint {
			BigSint {
				abs_value: &self.abs_value * &rhs.abs_value,
				is_negative: self.is_negative ^ rhs.is_negative,
			}
		}
	}

	impl BigSint {
		/// Performs the euclidian division `self / rhs`,
		/// returns `(quotient, remainder)`,
		/// `quotient` having the sign of `self` and `remainder` being positive.
		///
		/// Only accepts a (strictly) positive `rhs`.
		#[must_use]
		pub fn div_euclidian(&self, rhs: &BigSint) -> (BigSint, BigSint) {
			assert!(
				!(rhs.is_negative || rhs.is_zero()),
				"dividing a BigSint by zero or negative"
			);
			let (quotient, remainder) = self.abs_value.div_euclidian(&rhs.abs_value);
			(
				BigSint { abs_value: quotient, is_negative: self.is_negative },
				BigSint::from(remainder),
			)
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		/// A few signed integer values to iterate over in tests.
		fn some_values() -> impl Iterator<Item = i64> {
			let values: Vec<i64> = vec![
				0,
				1,
				8,
				17,
				42,
				69,
				100000000,
				123456789,
				0x123456789ABCDEF,
				u8::MAX as i64 - 1,
				u8::MAX as i64,
				u8::MAX as i64 + 1,
				u16::MAX as i64 - 123,
				u16::MAX as i64 - 1,
				u16::MAX as i64,
				u16::MAX as i64 + 1,
				u16::MAX as i64 + 123,
				u32::MAX as i64 - 123,
				u32::MAX as i64 - 1,
				u32::MAX as i64,
				u32::MAX as i64 + 1,
				u32::MAX as i64 + 123,
				i64::MAX - 123,
				i64::MAX - 1,
				i64::MAX,
			];
			values
				.clone()
				.into_iter()
				.map(|value| -value)
				.chain(values.into_iter())
		}

		#[test]
		fn preserve_value() {
			for value in some_values() {
				let value_before = value;
				let bs = BigSint::from(value_before);
				let value_after = i64::try_from(&bs).unwrap();
				assert_eq!(
					value_before, value_after,
					"converting to and then from a BigSint does not preserve the value"
				);
			}
		}

		#[test]
		fn too_negative_to_fit() {
			let too_negative_for_u32 = -8;
			let does_not_fit = u32::try_from(&BigSint::from(too_negative_for_u32));
			assert!(
				matches!(does_not_fit, Err(DoesNotFitInPrimitive)),
				"converting to u32 a BigSint that represents a value \
				too negative to fit in a u32 must fail"
			);
		}

		#[test]
		fn eq_with_itself() {
			for value in some_values() {
				let bs = BigSint::from(value);
				assert_eq!(bs, bs, "BigSint is not equal with itself");
			}
		}

		#[test]
		fn eq() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					let bs_b = BigSint::from(value_b);
					assert_eq!(
						value_a.eq(&value_b),
						bs_a.eq(&bs_b),
						"BigSint equality test behaves differently from Rust's"
					);
				}
			}
		}

		#[test]
		fn ord() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					let bs_b = BigSint::from(value_b);
					assert_eq!(
						value_a.cmp(&value_b),
						bs_a.cmp(&bs_b),
						"BigSint comparison behaves differently from Rust's"
					);
				}
			}
		}

		#[test]
		fn eq_consistent_with_ord() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					let bs_b = BigSint::from(value_b);
					assert_eq!(
						bs_a.eq(&bs_b),
						bs_a.cmp(&bs_b) == Ordering::Equal,
						"BigSint comparison (cmp) behaves differently BigSint equality test (eq)"
					);
				}
			}
		}

		#[test]
		fn add() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					let bs_b = BigSint::from(value_b);
					let big_sum_a_b = {
						let mut tmp = bs_a.clone();
						tmp += &bs_b;
						tmp
					};

					// Check against Rust's result, if available.
					if let Some(sum_a_b) = value_a.checked_add(value_b) {
						let sum_a_b_after = i64::try_from(&big_sum_a_b).expect(
							"the checked addition passed, thus this was expected to pass too",
						);
						assert_eq!(
							sum_a_b, sum_a_b_after,
							"BigSint addition behaves differently from Rusts's"
						);
					}

					// Check consistency accross all the `impl` blocks related to the addition.
					assert_eq!(
						big_sum_a_b,
						{
							let mut tmp = bs_a.clone();
							tmp += bs_b.clone();
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						&bs_a + &bs_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						&bs_a + bs_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						bs_a.clone() + &bs_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_sum_a_b,
						bs_a.clone() + bs_b.clone(),
						"one of the `impl`s is missbehaving"
					);
				}
			}
		}

		#[test]
		fn sub() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					let bs_b = BigSint::from(value_b);
					let big_subtration_a_b = {
						let mut tmp = bs_a.clone();
						tmp -= &bs_b;
						tmp
					};

					// Check against Rust's result, if available.
					if let Some(subtraction_a_b) = value_a.checked_sub(value_b) {
						let subtraction_a_b_after = i64::try_from(&big_subtration_a_b).unwrap();
						assert_eq!(
							subtraction_a_b, subtraction_a_b_after,
							"BigSint subctarction behaves differently from Rusts's"
						);
					}

					// Check consistency accross all the `impl` blocks related
					// to the subtraction.
					assert_eq!(
						big_subtration_a_b,
						{
							let mut tmp = bs_a.clone();
							tmp -= bs_b.clone();
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_subtration_a_b,
						&bs_a - &bs_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_subtration_a_b,
						&bs_a - bs_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_subtration_a_b,
						bs_a.clone() - &bs_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_subtration_a_b,
						bs_a.clone() - bs_b.clone(),
						"one of the `impl`s is missbehaving"
					);
				}
			}
		}

		#[test]
		fn mul() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					let bs_b = BigSint::from(value_b);
					let big_product_a_b = &bs_a * &bs_b;

					// Check against Rust's result, if available.
					if let Some(product_a_b) = value_a.checked_mul(value_b) {
						let product_a_b_after = i64::try_from(&big_product_a_b).expect(
							"the checked multiplication passed, \
							thus this was expected to pass too",
						);
						assert_eq!(
							product_a_b, product_a_b_after,
							"BigSint multiplication behaves differently from Rusts's"
						);
					}

					// Check consistency accross all the `impl` blocks related
					// to the multiplication.
					assert_eq!(
						big_product_a_b,
						&bs_a * bs_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						bs_a.clone() * &bs_b,
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						bs_a.clone() * bs_b.clone(),
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						{
							let mut tmp = bs_a.clone();
							tmp *= &bs_b;
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
					assert_eq!(
						big_product_a_b,
						{
							let mut tmp = bs_a.clone();
							tmp *= bs_b.clone();
							tmp
						},
						"one of the `impl`s is missbehaving"
					);
				}
			}
		}

		#[test]
		fn div_euclidian() {
			for value_a in some_values() {
				let bs_a = BigSint::from(value_a);
				for value_b in some_values() {
					// We don't do division by a negative number at the `BigSint` level.
					if value_b < 0 {
						continue;
					}
					// If Rust can't do the division (because `value_b` is zero), neither can we.
					if let Some(quotient_a_b) = value_a.checked_div(value_b) {
						let remainder_a_b = value_a % value_b;

						let bs_b = BigSint::from(value_b);
						let (big_quotient_a_b, big_remainder_a_b) = bs_a.div_euclidian(&bs_b);

						// Check against Rust's result.
						let quotient_a_b_after = i64::try_from(&big_quotient_a_b).unwrap();
						assert_eq!(
							quotient_a_b, quotient_a_b_after,
							"BigSint euclidian behaves differently from Rusts's for the quotient"
						);
						let remainder_a_b_after = i64::try_from(&big_remainder_a_b).unwrap();
						assert_eq!(
							remainder_a_b.abs(),
							remainder_a_b_after,
							"BigSint euclidian behaves differently from Rusts's \
							(when taking its absolute value) for the remainder"
						);
					}
				}
			}
		}

		#[test]
		#[should_panic]
		fn div_by_zero() {
			let _does_not_work = BigSint::from(69u64).div_euclidian(&BigSint::zero());
		}

		#[test]
		#[should_panic]
		fn div_by_strictly_negative() {
			// Expected to fail by design choice.
			let _does_not_work = BigSint::from(69u64).div_euclidian(&BigSint::from(-8i64));
		}
	}
}

mod big_frac {
	use std::{
		cmp::Ordering,
		ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign},
	};

	use super::{big_sint::BigSint, DoesNotFitInPrimitive};

	/// Big fraction (arbitrarly precise).
	///
	/// Is always simplified after creation or any operation.
	#[derive(Clone, Debug)]
	struct BigFrac {
		/// Numerator, bears the sign when simplified.
		num: BigSint,
		/// Denominator, must be positive when simplified.
		/// Must never be zero.
		/// Must be one if the numerator is zero and the fraction is simplified.
		den: BigSint,
	}

	impl BigFrac {
		fn zero() -> BigFrac {
			BigFrac { num: BigSint::zero(), den: BigSint::from(1) }
		}

		fn is_zero(&self) -> bool {
			self.num.is_zero()
		}

		/// Numerator.
		fn num(&self) -> &BigSint {
			&self.num
		}

		/// Denominator.
		///
		/// It is equal to one when the fraction is an integer (even zero).
		fn den(&self) -> &BigSint {
			&self.den
		}

		fn is_positive(&self) -> bool {
			self.num.is_positive()
		}

		fn is_integer(&self) -> bool {
			self.den == BigSint::from(1)
		}
	}

	impl From<BigSint> for BigFrac {
		fn from(value: BigSint) -> BigFrac {
			BigFrac { num: value, den: BigSint::from(1) }
		}
	}

	#[derive(Debug)]
	struct NotAnInteger;

	impl TryFrom<&BigFrac> for BigSint {
		type Error = NotAnInteger;
		fn try_from(value: &BigFrac) -> Result<BigSint, NotAnInteger> {
			if value.is_integer() {
				Ok(value.num.clone())
			} else {
				Err(NotAnInteger)
			}
		}
	}
	impl TryFrom<BigFrac> for BigSint {
		type Error = NotAnInteger;
		fn try_from(value: BigFrac) -> Result<BigSint, NotAnInteger> {
			if value.is_integer() {
				Ok(value.num)
			} else {
				Err(NotAnInteger)
			}
		}
	}

	impl<T: super::big_unit::LocalIntoU64> From<T> for BigFrac {
		fn from(value: T) -> BigFrac {
			BigFrac::from(BigSint::from(value))
		}
	}

	/// Rust forbids an other `impl From<T> for BigFrac` block even if T is bound
	/// to a trait that does not overlap with the trait bound of the previous block.
	/// This is because an hypothetical programmer could potentially sneak in and
	/// add `impl`s that make the two non-overlapping traits now overlapping.
	macro_rules! impl_from_into_i64 {
		($($primitive_type:ty),*) => {
			$(
				impl From<$primitive_type> for BigFrac {
					fn from(value: $primitive_type) -> BigFrac {
						let value: i64 = value.into();
						BigFrac::from(BigSint::from(value))
					}
				}
			)*
		}
	}
	impl_from_into_i64!(i8, i16, i32, i64);

	/// Greatest common divisor, expects `a` and `b` to be strictly positive.
	fn gcd(mut a: BigSint, mut b: BigSint) -> BigSint {
		if a < b {
			// We want `a` to be greater than `b`.
			gcd(b, a)
		} else {
			// Here `a` is greater than `b`.
			assert!(
				a.is_strictly_positive(),
				"`gcd` expects strictly positive arguments"
			);
			assert!(
				b.is_strictly_positive(),
				"`gcd` expects strictly positive arguments"
			);

			// See [https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclidean_algorithm].
			while !b.is_zero() {
				let a_mod_b = a.div_euclidian(&b).1;
				(a, b) = (b, a_mod_b);
			}
			a
		}
	}

	impl BigFrac {
		/// Simplify the fraction, making sure it is in a "valid" state.
		///
		/// A lot of methods expect `BigFrac`s in arguments to be simplified,
		/// and all the methods shall not allow `self` or a returned `BigFrac`
		/// to not be simplified.
		fn simplify(&mut self) {
			assert!(!self.den.is_zero());

			// The numerator must bear the sign.
			if self.den.is_strictly_negative() {
				self.den.negate();
				self.num.negate();
			}

			if self.num.is_zero() {
				// The denominator is expected to be one when the numerator is zero.
				self.den = BigSint::from(1);
			} else {
				// A fraction can be simplified by dividing both the numerator and the denominator
				// by their GCD. For example, 18 / 12 = (18/6) / (12/6) = 3 / 2, with GCD(18, 12) = 6.
				let gcd_num_den = gcd(self.num.clone().abs(), self.den.clone().abs());
				let (new_num, reminder_zero) = self.num.div_euclidian(&gcd_num_den);
				assert!(reminder_zero.is_zero());
				self.num = new_num;
				let (new_den, reminder_zero) = self.den.div_euclidian(&gcd_num_den);
				assert!(reminder_zero.is_zero());
				self.den = new_den;
			}
		}
	}

	impl BigFrac {
		fn from_num_and_den(num: impl Into<BigSint>, den: impl Into<BigSint>) -> BigFrac {
			let mut big_frac = BigFrac { num: num.into(), den: den.into() };
			assert!(
				!big_frac.den.is_zero(),
				"attempted to create a fraction with zero as the denominator"
			);
			big_frac.simplify();
			big_frac
		}
	}

	impl Eq for BigFrac {}
	impl PartialEq for BigFrac {
		fn eq(&self, rhs: &BigFrac) -> bool {
			(self.is_zero() && rhs.is_zero()) || (self.num == rhs.num && self.den == rhs.den)
		}
	}

	impl Ord for BigFrac {
		fn cmp(&self, rhs: &BigFrac) -> Ordering {
			// (a/b) < (c/d) iff (a*d) < (c*b) assuming non-zero denominators.
			(&self.num * &rhs.den).cmp(&(&rhs.num * &self.den))
		}
	}
	impl PartialOrd for BigFrac {
		fn partial_cmp(&self, rhs: &BigFrac) -> Option<Ordering> {
			Some(self.cmp(rhs))
		}
	}

	impl Add<&BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn add(self, rhs: &BigFrac) -> BigFrac {
			// (a/b) + (c/d) = (a*d)/(b*d) + (c*b)/(b*d) = (a*d + c*b)/(b*d)
			BigFrac::from_num_and_den(
				&self.num * &rhs.den + &rhs.num * &self.den,
				&self.den * &rhs.den,
			)
		}
	}
	impl Add<&BigFrac> for BigFrac {
		type Output = BigFrac;
		fn add(self, rhs: &BigFrac) -> BigFrac {
			&self + rhs
		}
	}
	impl Add<BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn add(self, rhs: BigFrac) -> BigFrac {
			self + &rhs
		}
	}
	impl Add<BigFrac> for BigFrac {
		type Output = BigFrac;
		fn add(self, rhs: BigFrac) -> BigFrac {
			&self + &rhs
		}
	}
	impl AddAssign<BigFrac> for BigFrac {
		fn add_assign(&mut self, rhs: BigFrac) {
			*self = &*self + &rhs
		}
	}
	impl AddAssign<&BigFrac> for BigFrac {
		fn add_assign(&mut self, rhs: &BigFrac) {
			*self = &*self + rhs
		}
	}

	impl Sub<&BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn sub(self, rhs: &BigFrac) -> BigFrac {
			// (a/b) - (c/d) = (a*d)/(b*d) - (c*b)/(b*d) = (a*d - c*b)/(b*d)
			BigFrac::from_num_and_den(
				&self.num * &rhs.den - &rhs.num * &self.den,
				&self.den * &rhs.den,
			)
		}
	}
	impl Sub<&BigFrac> for BigFrac {
		type Output = BigFrac;
		fn sub(self, rhs: &BigFrac) -> BigFrac {
			&self - rhs
		}
	}
	impl Sub<BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn sub(self, rhs: BigFrac) -> BigFrac {
			self - &rhs
		}
	}
	impl Sub<BigFrac> for BigFrac {
		type Output = BigFrac;
		fn sub(self, rhs: BigFrac) -> BigFrac {
			&self - &rhs
		}
	}
	impl SubAssign<BigFrac> for BigFrac {
		fn sub_assign(&mut self, rhs: BigFrac) {
			*self = &*self - &rhs
		}
	}
	impl SubAssign<&BigFrac> for BigFrac {
		fn sub_assign(&mut self, rhs: &BigFrac) {
			*self = &*self - rhs
		}
	}

	impl Mul<&BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn mul(self, rhs: &BigFrac) -> BigFrac {
			BigFrac {
				num: &self.num * &rhs.num,
				den: &self.den * &rhs.den,
			}
		}
	}
	impl Mul<&BigFrac> for BigFrac {
		type Output = BigFrac;
		fn mul(self, rhs: &BigFrac) -> BigFrac {
			&self * rhs
		}
	}
	impl Mul<BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn mul(self, rhs: BigFrac) -> BigFrac {
			self * &rhs
		}
	}
	impl Mul<BigFrac> for BigFrac {
		type Output = BigFrac;
		fn mul(self, rhs: BigFrac) -> BigFrac {
			&self * rhs
		}
	}
	impl MulAssign<BigFrac> for BigFrac {
		fn mul_assign(&mut self, rhs: BigFrac) {
			*self = &*self * &rhs;
		}
	}
	impl MulAssign<&BigFrac> for BigFrac {
		fn mul_assign(&mut self, rhs: &BigFrac) {
			*self = &*self * rhs;
		}
	}

	impl Div<&BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn div(self, rhs: &BigFrac) -> BigFrac {
			BigFrac {
				num: &self.num * &rhs.den,
				den: &self.den * &rhs.num,
			}
		}
	}
	impl Div<&BigFrac> for BigFrac {
		type Output = BigFrac;
		fn div(self, rhs: &BigFrac) -> BigFrac {
			&self / rhs
		}
	}
	impl Div<BigFrac> for &BigFrac {
		type Output = BigFrac;
		fn div(self, rhs: BigFrac) -> BigFrac {
			self / &rhs
		}
	}
	impl Div<BigFrac> for BigFrac {
		type Output = BigFrac;
		fn div(self, rhs: BigFrac) -> BigFrac {
			&self / rhs
		}
	}
	impl DivAssign<BigFrac> for BigFrac {
		fn div_assign(&mut self, rhs: BigFrac) {
			*self = &*self / &rhs;
		}
	}
	impl DivAssign<&BigFrac> for BigFrac {
		fn div_assign(&mut self, rhs: &BigFrac) {
			*self = &*self / rhs;
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;

		impl BigFrac {
			fn is_not_properly_simplified(&self) -> bool {
				if self.den.is_negative() {
					true
				} else if self.den.is_zero() {
					true
				} else if self.num.is_zero() && self.den != BigSint::from(1) {
					true
				} else {
					let mut simplified = self.clone();
					simplified.simplify();
					self.num != simplified.num || self.den != simplified.den
				}
			}
		}

		/// A few signed integer values to iterate over in tests.
		fn some_values() -> impl Iterator<Item = i64> {
			let values: Vec<i64> = vec![
				0,
				1,
				8,
				17,
				42,
				69,
				100000000,
				123456789,
				0x123456789ABCDEF,
				u8::MAX as i64 - 1,
				u8::MAX as i64,
				u8::MAX as i64 + 1,
				u16::MAX as i64 - 123,
				u16::MAX as i64 - 1,
				u16::MAX as i64,
				u16::MAX as i64 + 1,
				u16::MAX as i64 + 123,
				u32::MAX as i64 - 123,
				u32::MAX as i64 - 1,
				u32::MAX as i64,
				u32::MAX as i64 + 1,
				u32::MAX as i64 + 123,
				i64::MAX - 123,
				i64::MAX - 1,
				i64::MAX,
			];
			values
				.clone()
				.into_iter()
				.map(|value| -value)
				.chain(values.into_iter())
		}

		#[test]
		fn is_simplified_upon_creation() {
			for (value_a, value_b) in some_values().zip(some_values()) {
				if value_b == 0 {
					continue;
				}
				let bf_a_b = BigFrac::from_num_and_den(value_a, value_b);
				assert!(
					!bf_a_b.is_not_properly_simplified(),
					"BigFrac is not simplified upon creation from a num and den"
				);
			}
		}

		#[test]
		fn eq_with_itself() {
			for (value_a, value_b) in some_values().zip(some_values()) {
				if value_b == 0 {
					continue;
				}
				let bf_a_b = BigFrac::from_num_and_den(value_a, value_b);
				assert_eq!(bf_a_b, bf_a_b, "BigFrac is not equal with itself");
			}
		}

		#[test]
		fn simplify() {
			let bf = BigFrac::from_num_and_den(123456789, 987654321);
			assert!(
				bf.num == BigSint::from(13717421),
				"BigFrac not simplified upon creation from a num and den"
			);
			assert!(
				bf.den == BigSint::from(109739369),
				"BigFrac not simplified upon creation from a num and den"
			);
		}

		#[test]
		fn eq_consistent_with_ord() {
			for (value_a, value_b) in some_values().zip(some_values()) {
				if value_b == 0 {
					continue;
				}
				let bf_a_b = BigFrac::from_num_and_den(value_a, value_b);
				for (value_c, value_d) in some_values().zip(some_values()) {
					if value_d == 0 {
						continue;
					}
					let bf_c_d = BigFrac::from_num_and_den(value_c, value_d);
					assert_eq!(
						bf_a_b.eq(&bf_c_d),
						bf_a_b.cmp(&bf_c_d) == Ordering::Equal,
						"BigFrac comparison (cmp) behaves differently BigFrac equality test (eq)"
					);
				}
			}
		}

		#[test]
		fn add() {
			let bf_a = BigFrac::from_num_and_den(123456789, 987654321);
			let bf_b = BigFrac::from_num_and_den(999999999, 123454321);
			let bf_sum = bf_a + bf_b;
			let expected_result =
				BigFrac::from_num_and_den(111432843785686772u64, 13547799286863449u64);
			assert_eq!(bf_sum, expected_result);
		}

		#[test]
		fn sub() {
			let bf_a = BigFrac::from_num_and_den(123456789, 987654321);
			let bf_b = BigFrac::from_num_and_den(999999999, 123454321);
			let bf_subtraction = bf_a - bf_b;
			let expected_result =
				BigFrac::from_num_and_den(-108045893994834490i64, 13547799286863449i64);
			assert_eq!(bf_subtraction, expected_result);
		}

		#[test]
		fn mul() {
			let bf_a = BigFrac::from_num_and_den(123456789, 987654321);
			let bf_b = BigFrac::from_num_and_den(999999999, 123454321);
			let bf_product = bf_a * bf_b;
			let expected_result =
				BigFrac::from_num_and_den(13717420986282579u64, 13547799286863449u64);
			assert_eq!(bf_product, expected_result);
		}

		#[test]
		fn div() {
			let bf_a = BigFrac::from_num_and_den(123456789, 987654321);
			let bf_b = BigFrac::from_num_and_den(999999999, 123454321);
			let bf_division = bf_a / bf_b;
			let expected_result =
				BigFrac::from_num_and_den(1693474895426141u64, 109739368890260631u64);
			assert_eq!(bf_division, expected_result);
		}
	}
}
