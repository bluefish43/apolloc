# 🚀 Apolloc Compiler

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

🌐 `apolloc` is a compiler that translates source code written in Apollo into a native executable.

## 🔨 Work In Progress

This compiler is a work in progress. Although it is working, features may appear and disappear (although unlikely to) in the future without any previous warning. Use at your own risk.

## 💼 Features In List For Implementation

* Static function methods;
* Function templates (generics) using `scheme`; and
* Hot reloading.

## 🚦 Getting Started

### Prerequisites

- Have a 64-bits computer;
- Have `rustup` and `cargo` in the stable toolchain and the latest version
- Have `clang++` and `LLVM 18` installed in your system.

### Installation

```bash
git clone https://github.com/bluefish43/apolloc.git
cd apolloc
cargo build --release
```

## Usage

```bash
# For building an executable
# consider ./src/main.ap to be the input file
# consider ./out/main to be the output executable
# -O3 is the optimization flag. Use it from 0 to 3. It defaults to 1.
./target/release/apolloc build ./src/main.ap -o ./out/main -O3
```

## 📜 Language Syntax

Language syntax is described in its [separate file](./SYNTAX.md).

## 🌈 Examples

Examples file can be seen at their own [separate file](./EXAMPLES.md).

## 🤝 Contributing

We welcome contributions! If you'd like to contribute to the `apolloc` compiler, please check out our [Contribution Guidelines](./CONTRIBUTION.md).


