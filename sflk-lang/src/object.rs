use crate::{bignums::big_frac::BigFrac, sir::Block};
use std::{cmp::Ordering, collections::HashMap};

/// An `Object` is a value that can manipulated by SFLK code.
/// Every expression evaluates to an `Object`.
#[derive(Debug, Clone)]
pub enum Object {
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

#[derive(Debug)]
pub enum ObjectOperationError {
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
}

/// Hack to get the name of the current function, a Rust equivalent of C's `__func__`.
/// Pretty useful to avoid copy-pasted code to still contain the name of another function
/// in place of a string that is supposed to be the name of the function.
/// 
/// Code stolen from https://stackoverflow.com/a/40234666 (find the full path to the
/// function) and NOT upgraded to https://stackoverflow.com/a/63904992 (cut the path
/// to only keep the name of the function) because the full path makes finding the
/// function in the code a bit easiser.
macro_rules! function {
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

impl Object {
	pub fn type_name(&self) -> &'static str {
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
	pub fn logical_not(&self) -> Result<Object, ObjectOperationError> {
		match self {
			Object::Number(value) => Ok(Object::Number(BigFrac::from_bool(value.is_zero()))),
			obj => Err(ObjectOperationError::UnsupportedType {
				function_name: function!(),
				type_name: obj.type_name(),
			}),
		}
	}

	/// Returns the logical and of the given numbers intrepreted as booleans.
	pub fn logical_and(&self, rhs: &Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				let value = BigFrac::from_bool(!left_value.is_zero() && !right_value.is_zero());
				Ok(Object::Number(value))
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	/// Returns a number boolean that is true (non-zero) iff the given list of numbers
	/// is ordered (increasing) (strictly or not, depending of the parameter `strictly`).
	/// Lists of 0 or 1 numbers are considered to be ordered.
	pub fn is_ordered(&self, strictly: bool) -> Result<Object, ObjectOperationError> {
		match self {
			Object::List(vec) => {
				// Only numbers are supported by order test for now,
				// we make sure there is nothing else in the list.
				let not_a_number = vec.iter().find(|obj| !matches!(obj, Object::Number(_)));
				if let Some(obj) = not_a_number {
					return Err(ObjectOperationError::UnsupportedTypeInList {
						function_name: function!(),
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
						ordering_tester(left.cmp(&right))
					},
					_ => unreachable!(),
				});
				Ok(Object::Number(BigFrac::from_bool(is_ordered)))
			},
			obj => Err(ObjectOperationError::UnsupportedType {
				function_name: function!(),
				type_name: obj.type_name(),
			}),
		}
	}

	pub fn length(&self) -> Result<Object, ObjectOperationError> {
		match self {
			Object::List(vec) => Ok(Object::Number(BigFrac::from(vec.len() as u64))),
			Object::String(string) => {
				Ok(Object::Number(BigFrac::from(string.chars().count() as u64)))
			},
			obj => Err(ObjectOperationError::UnsupportedType {
				function_name: function!(),
				type_name: obj.type_name(),
			}),
		}
	}

	pub fn plus(self, rhs: Object) -> Result<Object, ObjectOperationError> {
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
				function_name: function!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	pub fn minus(self, rhs: Object) -> Result<Object, ObjectOperationError> {
		match (self, rhs) {
			(Object::Number(left_value), Object::Number(right_value)) => {
				Ok(Object::Number(left_value - right_value))
			},
			(Object::String(left_string), Object::String(right_string)) => {
				let value = BigFrac::from_bool(left_string != right_string);
				Ok(Object::Number(value))
			},
			(left, right) => Err(ObjectOperationError::UnsupportedPairOfTypes {
				function_name: function!(),
				left_type_name: left.type_name(),
				right_type_name: right.type_name(),
			}),
		}
	}

	// TODO: Move the rest of the object operations from `sir` to here.
}
