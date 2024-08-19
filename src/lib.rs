mod utils;

extern crate console_error_panic_hook;
extern crate web_sys;
use std::panic;

use wasm_bindgen::prelude::*;

const SCREEN_WIDTH:usize = 64;
const SCREEN_HEIGHT:usize = 32;
const INSTRUCTIONS_PER_SECOND:usize = 700;
const MEMORY_SIZE:u16 = 4096;
const START_ADDRESS:u16 = 512;


// A macro to provide `println!(..)`-style syntax for `console.log` logging.
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

#[wasm_bindgen]
pub struct ScreenBuffer {
	width: u8,
	height: u8,
	pixel_map: Vec<bool>,
}

impl ScreenBuffer {
	fn new(w:usize, h:usize) -> ScreenBuffer {
		return ScreenBuffer {
			width: w as u8,
			height: h as u8,
			pixel_map: vec![false; w * h]
		};
	}

	fn coords_to_index(&self, x: u8, y: u8) -> usize {
		return (y * self.width + x) as usize;
	}

	fn invert_pixel_at(&mut self, x: u8, y: u8) {
		if self.is_within_bounds(x, y) {
			let index = self.coords_to_index(x, y);
			self.pixel_map[index] = !self.pixel_map[index];
		}
	}

	fn get_pixel_at(&self, x: u8, y: u8) -> bool {
		if self.is_within_bounds(x, y) {
			return self.pixel_map[self.coords_to_index(x, y)];
		}
		else {
			return false;
		}
	}

	fn is_within_bounds(&self, x: u8, y: u8) -> bool {
		return (
			x >= 0 &&
			x < self.width &&
			y >= 0 &&
			y < self.height
		);
	}

	fn clear(&mut self) {
		for i in 0..(self.width * self.height) {
			self.pixel_map[i as usize] = false;
		}
	}
}

#[wasm_bindgen]
pub struct Memory {
	size_bytes: u16,
	memory: Vec<u8>,
}

impl Memory {
	fn new(size_bytes: u16) -> Memory {
		return Memory {
			size_bytes: size_bytes,
			memory: vec![0; size_bytes as usize],
		};
	}

	fn peek(&self, address: u16) -> u8 {
		return self.memory[address as usize];
	}

	fn poke(&mut self, address: u16, value: u8) {
		self.memory[address as usize] = value;
	}
}

#[wasm_bindgen]
pub struct C8Registers {
	program_counter: u16, // Actually 12 bits
	i: u16,
	v: [u8; 16], // v[0xf] is the flag register
	sound_timer: u8,
	delay_timer: u8,
	call_stack: Vec<u16>,
}

impl C8Registers {
	fn new() -> C8Registers {
		return C8Registers {
			program_counter: START_ADDRESS,
			i: 0,
			v: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
			sound_timer: 0,
			delay_timer: 0,
			call_stack: Vec::new(),
		};
	}

	fn push_pc(&mut self) {
		self.call_stack.push(self.program_counter);
	}

	fn pop_pc(&mut self) {
		//self.program_counter = self.call_stack.pop();
		match self.call_stack.pop() {
			Some(x) => self.program_counter = x,
			None => log!("Stack underflow!"), // This should probably be an error?
		}
	}

	fn skip_instruction(&mut self) {
		self.program_counter += 2;
	}

	fn wait_in_place(&mut self) {
		self.program_counter -= 2;
	}

	fn decrement_timers(&mut self) {
		if self.sound_timer > 0 {
			self.sound_timer -= 1;
		}

		if self.delay_timer > 0 {
			self.delay_timer -= 1;
		}
	}
}

#[wasm_bindgen]
pub struct Interpreter {
	memory: Memory,
	screen_buffer: ScreenBuffer,
	registers: C8Registers,
	keypad: Vec<bool>,
	key_just_pressed: Option<u8>,
}

#[wasm_bindgen]
impl Interpreter {
	pub fn new() -> Interpreter {
		console_error_panic_hook::set_once();

		let mut new_interpreter:Interpreter = Interpreter {
			memory: Memory::new(MEMORY_SIZE),
			screen_buffer: ScreenBuffer::new(SCREEN_WIDTH, SCREEN_HEIGHT),
			registers: C8Registers::new(),
			keypad: vec![false; 16],
			key_just_pressed: None,
		};

		// The first 512 bytes should be empty
		let mut TEST_GAME:Vec<u8> = vec![
			0x00, 0xe0, 0xa2, 0x2a, 0x60, 0x0c, 0x61, 0x08, 0xd0, 0x1f, 0x70, 0x09, 0xa2, 0x39, 0xd0, 0x1f, 
			0xa2, 0x48, 0x70, 0x08, 0xd0, 0x1f, 0x70, 0x04, 0xa2, 0x57, 0xd0, 0x1f, 0x70, 0x08, 0xa2, 0x66, 
			0xd0, 0x1f, 0x70, 0x08, 0xa2, 0x75, 0xd0, 0x1f, 0x12, 0x28, 0xff, 0x00, 0xff, 0x00, 0x3c, 0x00, 
			0x3c, 0x00, 0x3c, 0x00, 0x3c, 0x00, 0xff, 0x00, 0xff, 0xff, 0x00, 0xff, 0x00, 0x38, 0x00, 0x3f, 
			0x00, 0x3f, 0x00, 0x38, 0x00, 0xff, 0x00, 0xff, 0x80, 0x00, 0xe0, 0x00, 0xe0, 0x00, 0x80, 0x00, 
			0x80, 0x00, 0xe0, 0x00, 0xe0, 0x00, 0x80, 0xf8, 0x00, 0xfc, 0x00, 0x3e, 0x00, 0x3f, 0x00, 0x3b, 
			0x00, 0x39, 0x00, 0xf8, 0x00, 0xf8, 0x03, 0x00, 0x07, 0x00, 0x0f, 0x00, 0xbf, 0x00, 0xfb, 0x00, 
			0xf3, 0x00, 0xe3, 0x00, 0x43, 0xe0, 0x00, 0xe0, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 
			0x00, 0xe0, 0x00, 0xe0, 
		];

		new_interpreter.load_rom(TEST_GAME);
		return new_interpreter;
	}

	pub fn tick(&mut self, keypad_input:u16, key_pressed:i8) {
		for i in 0..16 {
			self.keypad[i] = (((keypad_input >> i) & 0b00000001) != 0);
		}

		if key_pressed >= 0 && key_pressed <= 15 {
			self.key_just_pressed = Some(key_pressed as u8);
		}
		else {
			self.key_just_pressed = None;
		}

		if false { // TODO: make this happen every 1/60th of a second when I implement cycle timing
			self.registers.decrement_timers();
		}

		let instruction: u16 = self.get_u16_at_pc();
		self.decode_and_execute(instruction);
	}

	pub fn screen_width(&self) -> u8 {
		return self.screen_buffer.width;
	}

	pub fn screen_height(&self) -> u8 {
		return self.screen_buffer.height;
	}

	pub fn screen_ptr(&self) -> *const bool {
		return self.screen_buffer.pixel_map.as_ptr();
	}

	pub fn load_rom(&mut self, rom:Vec<u8>) {
		for i in 0..rom.len() {
			self.memory.memory[START_ADDRESS as usize + i] = rom[i];
		}
	}
}

impl Interpreter {
	// -- Fetch --

	fn get_u8_at_pc(&mut self) -> u8 {
		let value:u8 = self.memory.peek(self.registers.program_counter as u16);
		self.registers.program_counter += 1;
		return value;
	}

	fn get_u16_at_pc(&mut self) -> u16 {
		// Not sure about endianness. This may need rewritten.
		let mut memory_value: u16 = self.get_u8_at_pc() as u16;
		memory_value = memory_value << 8;
		memory_value += self.get_u8_at_pc() as u16;

		return memory_value;
	}

	fn get_u8_at_i(&mut self) -> u8 {
		let value:u8 = self.memory.peek(self.registers.i);
		self.registers.i += 1;
		return value;
	}

	fn set_u8_at_i(&mut self, value: u8) {
		self.memory.poke(self.registers.i, value);
		self.registers.i += 1;
	}


	// -- Decode --

	fn decode_and_execute(&mut self, instruction: u16) {
		let nibbles: [u8; 4] = get_nibbles(instruction);
		let last_3_nibbles: u16 = merge_3_nibbles(nibbles[1], nibbles[2], nibbles[3]);
		let last_byte: u8 = merge_2_nibbles(nibbles[2], nibbles[3]);

		log!("{}", instruction);

		match nibbles[0] {
			0x0 => {
				if instruction == 0x00e0 {
					self.inst_clear();
				}
				else if instruction == 0x00ee {
					self.inst_return();
				}
			},
			0x1 => self.inst_jump(last_3_nibbles),
			0x2 => self.inst_call(last_3_nibbles),
			0x3 => self.inst_if_vx_neq_val(nibbles[1], last_byte),
			0x4 => self.inst_if_vx_eq_val(nibbles[1], last_byte),
			0x5 => self.inst_if_vx_neq_vy(nibbles[1], nibbles[2]),
			0x6 => self.inst_set_vx_to_val(nibbles[1], last_byte),
			0x7 => self.inst_vx_add_val(nibbles[1], last_byte),
			0x8 => self.decode_register_op_inst(nibbles[1], nibbles[2], nibbles[3]),
			0x9 => self.inst_if_vx_eq_vy(nibbles[1], nibbles[2]),
			0xA => self.inst_set_i_to_val(last_3_nibbles),
			0xB => self.inst_jump0(last_3_nibbles),
			0xC => self.inst_randomize_vx(nibbles[1], last_byte),
			0xD => self.inst_sprite(nibbles[1], nibbles[2], nibbles[3]),
			0xE => {
				if last_byte == 0x9e {
					self.inst_if_key_not_pressed(nibbles[1]);
				}
				else if last_byte == 0xa1 {
					self.inst_if_key_pressed(nibbles[1]);
				}
			},
			0xF => self.decode_misc_inst(nibbles[1], last_byte),
			_ => {},
		}
	}

	fn decode_register_op_inst(&mut self, reg_x: u8, reg_y: u8, operation: u8) {
		match operation {
			0x0 => self.inst_v_op_set(reg_x, reg_y),
			0x1 => self.inst_v_op_bitwise_or(reg_x, reg_y),
			0x2 => self.inst_v_op_bitwise_and(reg_x, reg_y),
			0x3 => self.inst_v_op_bitwise_xor(reg_x, reg_y),
			0x4 => self.inst_v_op_add(reg_x, reg_y),
			0x5 => self.inst_v_op_subtract(reg_x, reg_y),
			0x6 => self.inst_v_op_shift_right(reg_x, reg_y),
			0x7 => self.inst_v_op_set_negative(reg_x, reg_y),
			0xE => self.inst_v_op_shift_left(reg_x, reg_y),
			_ => {},
		}
	}

	fn decode_misc_inst(&mut self, register: u8, operation: u8) {
		match operation {
			0x07 => self.inst_set_vx_to_delay(register),
			0x0A => self.inst_wait_for_keypress(register),
			0x15 => self.inst_set_delay_to_vx(register),
			0x18 => self.inst_set_buzzer_to_vx(register),
			0x1E => self.inst_add_vx_to_i(register),
			0x29 => self.inst_set_i_to_hex_char(register),
			0x33 => self.inst_vx_to_bcd(register),
			0x55 => self.inst_save_v0_through_vx(register),
			0x65 => self.inst_load_v0_through_vx(register),
			_ => {},
		}
	}


	// -- Execute --

	// 00E0
	fn inst_clear(&mut self) {
		self.screen_buffer.clear();
	}

	// 00EE
	fn inst_return(&mut self) {
		self.registers.pop_pc();
	}

	// 1nnn
	fn inst_jump(&mut self, jump_address: u16) {
		self.registers.program_counter = jump_address;
	}

	// 2nnn
	fn inst_call(&mut self, subroutine_address: u16) {
		self.registers.push_pc();
		self.registers.program_counter = subroutine_address;
	}

	// 3xnn
	fn inst_if_vx_neq_val(&mut self, register: u8, value: u8) {
		if self.registers.v[register as usize] == value {
			self.registers.skip_instruction();
		}
	}

	// 4xnn
	fn inst_if_vx_eq_val(&mut self, register: u8, value: u8) {
		if self.registers.v[register as usize] != value {
			self.registers.skip_instruction();
		}
	}

	// 5xy0
	fn inst_if_vx_neq_vy(&mut self, reg_x: u8, reg_y: u8) {
		if self.registers.v[reg_x as usize] == self.registers.v[reg_y as usize] {
			self.registers.skip_instruction();
		}
	}

	// 6xnn
	fn inst_set_vx_to_val(&mut self, register: u8, value: u8) {
		self.registers.v[register as usize] = value;
	}

	// 7xnn
	fn inst_vx_add_val(&mut self, register: u8, value: u8) {
		self.registers.v[register as usize] += value;
	}

	// 8xy0
	fn inst_v_op_set(&mut self, reg_x: u8, reg_y: u8) {
		self.registers.v[reg_x as usize] = self.registers.v[reg_y as usize];
	}

	// 8xy1
	fn inst_v_op_bitwise_or(&mut self, reg_x: u8, reg_y: u8) {
		self.registers.v[reg_x as usize] = self.registers.v[reg_x as usize] | self.registers.v[reg_y as usize];
	}

	// 8xy2
	fn inst_v_op_bitwise_and(&mut self, reg_x: u8, reg_y: u8) {
		self.registers.v[reg_x as usize] = self.registers.v[reg_x as usize] & self.registers.v[reg_y as usize];
	}

	// 8xy3
	fn inst_v_op_bitwise_xor(&mut self, reg_x: u8, reg_y: u8) {
		self.registers.v[reg_x as usize] = self.registers.v[reg_x as usize] ^ self.registers.v[reg_y as usize];
	}

	// 8xy4
	fn inst_v_op_add(&mut self, reg_x: u8, reg_y: u8) {
		let vx_val:u8 = self.registers.v[reg_x as usize];
		let vy_val:u8 = self.registers.v[reg_y as usize];

		// vf = 1 on overflow
		if (vx_val as usize) + (vy_val as usize) > 255 {
			self.registers.v[0xF] = 1;
		}
		else {
			self.registers.v[0xF] = 0;
		}

		self.registers.v[reg_x as usize] += vy_val;
	}

	// 8xy5
	fn inst_v_op_subtract(&mut self, reg_x: u8, reg_y: u8) {
		// vf = 0 on underflow
		self.registers.v[0xF] = 1;
		if self.registers.v[reg_x as usize] > self.registers.v[reg_y as usize] {
			self.registers.v[0xF] = 0;
		}

		self.registers.v[reg_x as usize] -= self.registers.v[reg_y as usize];
	}

	// 8xy6
	fn inst_v_op_shift_right(&mut self, reg_x: u8, reg_y: u8) {
		// vf = least significant bit before shift (the bit shifted out)
		self.registers.v[0xF] = self.registers.v[reg_y as usize] & 0b00000001;

		self.registers.v[reg_x as usize] = self.registers.v[reg_y as usize] >> 1;
	}

	// 8xy7
	fn inst_v_op_set_negative(&mut self, reg_x: u8, reg_y: u8) {
		// vf = 0 on underflow
		self.registers.v[0xF] = 1;
		if self.registers.v[reg_y as usize] > self.registers.v[reg_x as usize] {
			self.registers.v[0xF] = 0;
		}

		self.registers.v[reg_x as usize] = self.registers.v[reg_y as usize] - self.registers.v[reg_x as usize];
	}

	// 8xyE
	fn inst_v_op_shift_left(&mut self, reg_x: u8, reg_y: u8) {
		// vf = most significant bit before shift (the bit shifted out)
		self.registers.v[0xF] = self.registers.v[reg_y as usize] & 0b10000000;

		self.registers.v[reg_x as usize] = self.registers.v[reg_y as usize] << 1;
	}

	// 9xy0
	fn inst_if_vx_eq_vy(&mut self, reg_x: u8, reg_y: u8) {
		if self.registers.v[reg_x as usize] != self.registers.v[reg_y as usize] {
			self.registers.skip_instruction();
		}
	}

	// Annn
	fn inst_set_i_to_val(&mut self, value: u16) {
		self.registers.i = value;
	}

	// Bnnn
	fn inst_jump0(&mut self, address: u16) {
		self.registers.program_counter = address + self.registers.v[0] as u16;
	}

	// Cxnn
	fn inst_randomize_vx(&mut self, register: u8, and_value: u8) {
		let random_byte:u8 = 0; // TODO: implement this. rand crate doesn't work with wasm. Need to get random number through js?
		self.registers.v[register as usize] = (random_byte & and_value);
	}

	// Dxyn
	fn inst_sprite(&mut self, reg_x: u8, reg_y: u8, spr_height: u8) {
		log!("Drawing Sprite!");
		// Get screen coordinates to draw the sprite from registers.
		// The start position for drawing the sprite wraps around the screen,
		// but the individual pixels drawn don't. Why? Ask the guy who invented
		// Chip-8 (he's probably dead).
		let start_x:u8 = self.registers.v[reg_x as usize] % self.screen_buffer.width;
		let start_y:u8 = self.registers.v[reg_y as usize] % self.screen_buffer.height;

		self.registers.v[0xF] = 0;
		
		let mut i_offset:u8 = 0;
		for iy in 0..spr_height {
			let spr_data:u8 = self.memory.peek(self.registers.i + i_offset as u16);

			for ix in 0..8 {
				let pixel_x:u8 = start_x + (ix as u8);
				let pixel_y:u8 = start_y + (iy as u8);

				log!("Pixel at {}, {}", pixel_x, pixel_y);

				if self.screen_buffer.is_within_bounds(pixel_x, pixel_y) {
					let spr_bit:bool = ((spr_data >> ix) & 0b00000001) != 0; // Whew.
					
					if spr_bit {
						if self.screen_buffer.get_pixel_at(pixel_x, pixel_y) {
							self.registers.v[0xF] = 1;
						}

						// Draw the pixel
						self.screen_buffer.invert_pixel_at(pixel_x, pixel_y);
						log!("Drew pixel!");
					}
				}
			}

			i_offset += 1;
		}
	}

	// Ex9E
	fn inst_if_key_not_pressed(&mut self, register: u8) {
		let keypad_key:usize = self.registers.v[register as usize] as usize;
		
		if self.keypad[keypad_key] {
			self.registers.skip_instruction();
		}
	}

	// ExA1
	fn inst_if_key_pressed(&mut self, register: u8) {
		let keypad_key:usize = self.registers.v[register as usize] as usize;
		
		if !self.keypad[keypad_key] {
			self.registers.skip_instruction();
		}
	}

	// Fx07
	fn inst_set_vx_to_delay(&mut self, register: u8) {
		self.registers.v[register as usize] = self.registers.delay_timer;
	}

	// Fx0A
	// This is a blocking instruction (cursed).
	fn inst_wait_for_keypress(&mut self, register: u8) {
		match self.key_just_pressed {
			Some(key) => self.registers.v[register as usize] = key,
			None => self.registers.wait_in_place(), // Back program counter up 2 bytes
		}
	}

	// Fx15
	fn inst_set_delay_to_vx(&mut self, register: u8) {
		self.registers.delay_timer = self.registers.v[register as usize];
	}

	// Fx18
	fn inst_set_buzzer_to_vx(&mut self, register: u8) {
		self.registers.sound_timer = self.registers.v[register as usize];
	}

	// Fx1E
	fn inst_add_vx_to_i(&mut self, register: u8) {
		self.registers.i += self.registers.v[register as usize] as u16;
	}

	// Fx29
	fn inst_set_i_to_hex_char(&mut self, register: u8) {
		// Wat?
		// TODO
	}

	// Fx33
	fn inst_vx_to_bcd(&mut self, register: u8) {
		let value:u8 = self.registers.v[register as usize];

		// TODO: test if this is correct.
		self.set_u8_at_i(value / 100);
		self.set_u8_at_i((value / 10) % 10);
		self.set_u8_at_i(value % 10);
	}

	// Fx55
	fn inst_save_v0_through_vx(&mut self, register: u8) {
		for i in 0..register {
			self.set_u8_at_i(self.registers.v[i as usize]);
		}
	}

	//Fx65
	fn inst_load_v0_through_vx(&mut self, register: u8) {
		for i in 0..register {
			self.registers.v[i as usize] = self.get_u8_at_i();
		}
	}
}

fn get_nibbles(word: u16) -> [u8; 4] {
	let nib1: u8 = ((word >> 12) & 0xf) as u8;
	let nib2: u8 = ((word >> 8) & 0xf) as u8;
	let nib3: u8 = ((word >> 4) & 0xf) as u8;
	let nib4: u8 = (word & 0xf) as u8;

	return [nib1, nib2, nib3, nib4];
}

fn merge_2_nibbles(nib1: u8, nib2: u8) -> u8 {
	return ((nib1 << 4) + nib2);
}

fn merge_3_nibbles(nib1: u8, nib2: u8, nib3: u8) -> u16 {
	return (
		((nib1 as u16) << 8) +
		((nib2 as u16) << 4) +
		(nib3 as u16)
	);
}


