use crate::{
	bignums::{
		big_frac::{BigFrac, NotAnInteger},
		big_sint::BigSint,
		DoesNotFitInPrimitive,
	},
	sir::Block,
};
use std::{cmp::Ordering, collections::HashMap};

/// An `Object` is a value that can manipulated by SFLK code.
/// Every expression evaluates to an `Object`.
#[derive(Debug, Clone)]
pub(crate) enum Object {
	/// The `()` literal is an expression that evaluates to that.
	Nothing,
	Number(BigFrac),
	String(String),
	/// A block of code literal is an expression that evaluates to that,
	/// without being executed (it may be executed later, but
	/// not by the evaluation of the literal itself).
	///
	/// Here is an example of a block of code literal:
	/// `{pr "SFLK better than Python" nl v < 69}`
	Block(Block),
	List(Vec<Object>),
	/// A context object is a dictionary of variable names and their values.
	/// A context object can be obtained from a context (from the context tree
	/// that holds data about actual variables) via the `cx` keyword, and
	/// can be injected in a context (from the context tree) via the `cy` keyword.
	Context(HashMap<String, Object>),
}

/// Error that occured during an operation on `Object`s.
///
/// The `function_name` field common across variants is supposed to be filled with the
/// name of the `Object` method that raised the error (using `function_name!()` makes
/// code safe for copy-pasting error construction without having to think about
/// changing the function name).
#[derive(Debug)]
// The fields ARE used when their values are printed by `unwrap`, but for some reason
// (see [https://github.com/rust-lang/rust/issues/88900]) the compiler says they are not.
#[allow(unused)]
pub(crate) enum ObjectOperationError {
	/// An unary operation did not support the given type.
	UnsupportedType {
		function_name: &'static str,
		type_name: &'static str,
	},
	/// A binary operation did not support the given pair of types.
	UnsupportedPairOfTypes {
		function_name: &'static str,
		left_type_name: &'static str,
		right_type_name: &'static str,
	},
	/// An unary operation on a list did not support to find a
	/// certain type in the list.
	UnsupportedTypeInList {
		function_name: &'static str,
		type_name: &'static str,
	},
	/// An operation did not support a number not because of its type
	/// but because of its value that did not verify some expected property.
	UnsupportedNumber {
		function_name: &'static str,
		number_value: BigFrac,
		error: UnsupportedNumberError,
	},
	NumberIndexOutOfRange {
		function_name: &'static str,
		index_value: usize,
		range_min_included: usize,
		range_max_included: usize,
	},
	StringIndexNotFound {
		function_name: &'static str,
		index_value: String,
	},
}

#[derive(Debug)]
pub(crate) enum UnsupportedNumberError {
	DoesNotFitInPrimitive(DoesNotFitInPrimitive),
	NotAnInteger(NotAnInteger),
}

/// Hack to get the name of the current function, a Rust equivalent of C's `__func__`.
/// Pretty useful to avoid copy-pasted code to still contain the name of another function
/// in place of a string that is supposed to be the name of the function.
///
/// Code stolen from https://stackoverflow.com/a/40234666 (find the full path to the
/// function) and NOT upgraded to https://stackoverflow.com/a/63904992 (cut the path
/// to only keep the name of the function) because the full path makes finding the
/// function in the code a bit easiser.
macro_rules! function_name {
	() => {{
		// Define a sub-function.
		fn f() {}
		// Find the name of the sub-function (that is full-path-to-function + "::f").
		let name = {
			fn type_name_of<T>(_: T) -> &'static str {
				std::any::type_name::<T>()
			}
			type_name_of(f)
		};
		// Cut out the "::f".
		&name[..name.len() - 3]
	}};
}

fn convert_big_frac_to_integer_primitive<PrimitiveType>(
	frac: BigFrac,
	caller_function_name: &'static str,
) -> Result<PrimitiveType, ObjectOperationError>
where
	PrimitiveType: for<'a> TryFrom<&'a BigSint, Error = DoesNotFitInPrimitive>,
{
	let right_as_integer = match BigSint::try_from(&frac) {
		Ok(value) => value,
		Err(error @ NotAnInteger) => {
			return Err(ObjectOperationError::UnsupportedNumber {
				function_name: caller_function_name,
				number_value: frac,
				error: UnsupportedNumberError::NotAnInteger(error),
			})
		},
	};
	let right_as_integer_primitive = match PrimitiveType::try_from(&right_as_integer) {
		Ok(value) => value,
		Err(error) => {
			return Err(ObjectOperationError::UnsupportedNumber {
				function_name: caller_function_name,
				number_value: frac,
				error: UnsupportedNumberError::DoesNotFitInPrimitive(error),
			})
		},
	};
	Ok(right_as_integer_primitive)
}

impl Object {
	pub(crate) fn type_name(&self) -> &'static str {
		match self {
			Object::Nothing => "nothing",
			Object::Number(_) => "number",
			Object::String(_) => "string",
			Object::Block(_) => "block",
			Object::List(_) => "list",
			Object::Context(_) => "context",
		}
	}

	/// Returns the logical not of the given number intrepreted as a boolean.
	pub(crate) fn logical_not(&self) -> Result<Object, ObjectOperationError> {
		match self {
			Object::Number(value) => Ok(Object::Number(BigFrac::from_bool(value.is_zero()))),
			obj => Err(ObjectOperationError::UnsupportedType {
				function_name: function_name!(),
				type_name: obj.type_name(),
			}),
		}
	}

	/// Returns the logical and of the given numbers intrepreted as booleans.
	pub(crate) fn logical_and(&self, rhs: &Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				let value = BigFrac::from_bool(!left_value.is_zero() && !right_value.is_zero());
				Ok(Object::Number(value))
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function_name!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	/// Returns a number boolean that is true (non-zero) iff the given list of numbers
	/// is ordered (increasing) (strictly or not, depending of the parameter `strictly`).
	/// Lists of 0 or 1 numbers are considered to be ordered.
	pub(crate) fn is_ordered(&self, strictly: bool) -> Result<Object, ObjectOperationError> {
		match self {
			Object::List(vec) => {
				// Only numbers are supported by order test for now,
				// we make sure there is nothing else in the list.
				let not_a_number = vec.iter().find(|obj| !matches!(obj, Object::Number(_)));
				if let Some(obj) = not_a_number {
					return Err(ObjectOperationError::UnsupportedTypeInList {
						function_name: function_name!(),
						type_name: obj.type_name(),
					});
				}

				let ordering_tester = if strictly {
					Ordering::is_lt
				} else {
					Ordering::is_le
				};
				let is_ordered = vec.as_slice().windows(2).all(|window| match window {
					[Object::Number(left), Object::Number(right)] => {
						ordering_tester(left.cmp(right))
					},
					_ => unreachable!(),
				});
				Ok(Object::Number(BigFrac::from_bool(is_ordered)))
			},
			obj => Err(ObjectOperationError::UnsupportedType {
				function_name: function_name!(),
				type_name: obj.type_name(),
			}),
		}
	}

	pub(crate) fn length(&self) -> Result<Object, ObjectOperationError> {
		match self {
			Object::List(vec) => Ok(Object::Number(BigFrac::from(vec.len() as u64))),
			Object::String(string) => {
				Ok(Object::Number(BigFrac::from(string.chars().count() as u64)))
			},
			obj => Err(ObjectOperationError::UnsupportedType {
				function_name: function_name!(),
				type_name: obj.type_name(),
			}),
		}
	}

	pub(crate) fn plus(self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				Ok(Object::Number(left_value + right_value))
			},
			(Object::String(left_string), Object::String(right_string)) => {
				Ok(Object::String(left_string + &right_string))
			},
			(Object::Block(left_block), Object::Block(right_block)) => {
				Ok(Object::Block(left_block.concat(right_block)))
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function_name!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	pub(crate) fn minus(self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				Ok(Object::Number(left_value - right_value))
			},
			(Object::String(left_string), Object::String(right_string)) => {
				let value = BigFrac::from_bool(left_string != right_string);
				Ok(Object::Number(value))
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function_name!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	pub(crate) fn star(self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				Ok(Object::Number(left_value * right_value))
			},
			(Object::String(left_string), Object::Number(right_value)) => {
				let right_as_usize =
					convert_big_frac_to_integer_primitive::<u64>(right_value, function_name!())?
						as usize;
				Ok(Object::String(left_string.repeat(right_as_usize)))
			},
			(Object::Block(left_block), Object::Number(right_value)) => {
				let mut block = left_block.clone();
				let right_as_u64 =
					convert_big_frac_to_integer_primitive::<u64>(right_value, function_name!())?;
				for _ in 0..right_as_u64 {
					block = block.concat(left_block.clone());
				}
				Ok(Object::Block(block))
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function_name!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	pub(crate) fn slash(self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				Ok(Object::Number(left_value / right_value))
			},
			(Object::String(left_string), Object::String(right_string)) => Ok(Object::Number(
				BigFrac::from(left_string.matches(right_string.as_str()).count() as u64),
			)),
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function_name!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	pub(crate) fn comma(self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Nothing, right) => Ok(Object::List(vec![right])),
			(Object::List(mut vec), right) => {
				vec.push(right);
				Ok(Object::List(vec))
			},
			(left, _right) => Err(ObjectOperationError::UnsupportedType {
				function_name: function_name!(),
				type_name: left.type_name(),
			}),
		}
	}

	pub(crate) fn double_comma(self, rhs: Object) -> Object {
		Object::List(vec![self, rhs])
	}

	/// Returns `self[rhs]`.
	pub(crate) fn index(&self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::List(vec), Object::Number(index)) => {
				let index_as_usize =
					convert_big_frac_to_integer_primitive::<u64>(index, function_name!())? as usize;
				match vec.get(index_as_usize) {
					Some(value) => Ok(value.clone()),
					None => Err(ObjectOperationError::NumberIndexOutOfRange {
						function_name: function_name!(),
						index_value: index_as_usize,
						range_min_included: 0,
						range_max_included: vec.len() - 1,
					}),
				}
			},
			(Object::String(string), Object::Number(index)) => {
				let index_as_usize =
					convert_big_frac_to_integer_primitive::<u64>(index, function_name!())? as usize;
				match string.chars().nth(index_as_usize) {
					Some(value) => Ok(Object::String(value.to_string())),
					None => Err(ObjectOperationError::NumberIndexOutOfRange {
						function_name: function_name!(),
						index_value: index_as_usize,
						range_min_included: 0,
						range_max_included: string.chars().count() - 1,
					}),
				}
			},
			(Object::Context(var_table), Object::String(string_index)) => {
				match var_table.get(&string_index) {
					Some(value) => Ok(value.clone()),
					None => Err(ObjectOperationError::StringIndexNotFound {
						function_name: function_name!(),
						index_value: string_index,
					}),
				}
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function_name!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}
}
