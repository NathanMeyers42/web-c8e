import * as wasm from "chip-8-emulator";
import { Interpreter } from "chip-8-emulator";
import { memory } from "chip-8-emulator/chip_8_emulator_bg";

const OFF_COLOR = "#222222";
const ON_COLOR = "#EEEEEE";

const interpreter = Interpreter.new();

const SCREEN_WIDTH = interpreter.screen_width();
const SCREEN_HEIGHT = interpreter.screen_height();
const PIXEL_SIZE = 16;

const canvas = document.getElementById("c8-screen-canvas");
canvas.width = SCREEN_WIDTH * PIXEL_SIZE;
canvas.height = SCREEN_HEIGHT * PIXEL_SIZE;
const ctx = canvas.getContext("2d");

const mainLoop = () => {
	interpreter.tick(0, -1);
	drawScreen();

	requestAnimationFrame(mainLoop);
};

const drawScreen = () => {
	const screenPtr = interpreter.screen_ptr();
	const screen = new Uint8Array(memory.buffer, screenPtr, SCREEN_WIDTH * SCREEN_HEIGHT);

	ctx.beginPath();

	for (let row = 0; row < SCREEN_HEIGHT; row++) {
		for (let col = 0; col < SCREEN_WIDTH; col++) {
			const index = row * SCREEN_HEIGHT + col;

			ctx.fillStyle = screen[index] === 1
				? ON_COLOR
				: OFF_COLOR;

			ctx.fillRect(
				col * PIXEL_SIZE,
				row * PIXEL_SIZE,
				PIXEL_SIZE,
				PIXEL_SIZE
			);
		}
	}

	ctx.stroke();
};

requestAnimationFrame(mainLoop);