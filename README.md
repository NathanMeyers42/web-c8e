This is an unfinished Chip-8 emulator made to run in a web browser using WASM-Pack for rust. Relevant code is in src/lib.rs and www/index.js.

Some day, I may finish this and turn it into a proper web application.

To run on Windows:
  
  Run the following commands in powershell:
  
    wasm-pack build
  
    $env:NODE_OPTIONS = "--openssl-legacy-provider"
  
    cd www
  
    npm run start

  Then open localhost:8080 in your web browser of choice.