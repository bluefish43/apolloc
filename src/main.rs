#![allow(dead_code)]

static mut CURRENT_FILE: &str = "";
static mut CURRENT_FILE_NAME: &str = "default.ap";

mod backend;
mod frontend;

extern crate llvm_sys;

use std::process::ExitCode;
use std::ptr::null_mut;

use ansi_term::Color;
use anyhow::anyhow;
use backend::builder::ApolloBuilder;
use backend::llvm::initialize_native_asm_printer;
use backend::llvm::initialize_native_target;
use backend::llvm::Context;
use clap::Arg;
use clap::Command;
use clap::FromArgMatches;
use clap::Parser;
use frontend::checker::Checker;
use frontend::tokens::Tokenizer;
use libc::{signal, kill, SIGABRT, SIGINT, SIGSEGV};
use std::process;

#[macro_export]
macro_rules! c_str {
    ($arg:expr) => {
        format!("{}\0", $arg).as_ptr() as *const i8
    };
    ($($arg:expr),*) => {
        format!("{}\0", format!($($arg),*)).as_ptr() as *const i8
    }
}

macro_rules! generate_cfg {
    ($(($arch:literal, $os:literal, $env:literal)),*) => {
        $(
            #[cfg(all(
                target_arch = $arch,
                target_os = $os,
                target_env = $env
            ))]
            return concat!($arch, "-", $os, "-", $env);
        )*

        // Default case if no supported target triple is matched
        return "unknown-unknown-unknown";
    };
}

// A function that returns one of the available target triples
fn get_target_triple() -> &'static str {
    generate_cfg!(
        ("x86_64", "linux", "gnu"),
        ("x86_64", "windows", "msvc"),
        ("aarch64", "ios", ""),
        ("arm", "linux", "gnueabihf"),
        ("x86", "linux", "gnu"),
        ("mips", "linux", "gnu"),
        ("powerpc", "linux", "gnu"),
        ("x86_64", "macos", ""),
        ("aarch64", "android", ""),
        ("sparc", "solaris", ""),
        ("riscv64", "linux", "gnu"),
        ("x86", "windows", "msvc"),
        ("arm", "windows", "msvc"),
        ("aarch64", "windows", "msvc"),
        ("mips", "windows", "msvc"),
        ("powerpc", "windows", "msvc"),
        ("x86_64", "freebsd", ""),
        ("aarch64", "freebsd", ""),
        ("arm", "freebsd", ""),
        ("x86", "freebsd", ""),
        ("x86_64", "openbsd", ""),
        ("aarch64", "openbsd", ""),
        ("arm", "openbsd", ""),
        ("mips", "openbsd", ""),
        ("x86", "openbsd", "")
    );
}

lazy_static::lazy_static! {
    static ref VERBOSE_STRING: String = ansi_term::Color::Green.paint("VERBOSE").to_string();
}

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    #[clap(subcommand)]
    subcommand: Subcommands,
}

#[derive(clap::Parser, Debug)]
enum Subcommands {
    /// This builds the source file into an output executable.
    Build {
        /// This is the input source file.
        input_file: String,
        /// This is the output file.
        #[arg(short, long)]
        output_file: String,
        /// What to output
        #[arg(long)]
        output_type: Option<String>,
        /// Target triple to use
        #[arg(long)]
        target_triple: Option<String>,
        #[clap(short = 'O', long = "option", default_value = "0")]
        optimization: u8,
        #[arg(long, short)]
        verbose: bool,
        #[arg(long, short)]
        noclear: bool,
    },
    Print {
        what_to_print: String,
    },
}

extern "C" fn handle_signal(sig: i32) {
    match sig {
        SIGSEGV => generate_stack_dump("Segmentation fault"),
        SIGABRT => generate_stack_dump("Aborted"),
        SIGINT => generate_stack_dump("Interrupted"),
        _ => generate_stack_dump("Unknown signal"),
    }
    process::exit(3)
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    unsafe {
        signal(SIGSEGV, handle_signal as usize);
        signal(SIGABRT, handle_signal as usize);
        signal(SIGINT, handle_signal as usize);
    }

    match args.subcommand {
        Subcommands::Build {
            input_file,
            output_file,
            output_type,
            target_triple,
            optimization,
            verbose,
            noclear,
        } => {
            if optimization > 3 {
                return Err(anyhow!("Optimization level must be between 0 and 3"));
            }
            let target_triple = target_triple.unwrap_or(get_target_triple().to_string());
            initialize_native_target()?;
            initialize_native_asm_printer()?;

            if verbose {
                eprintln!("{} Reading input file `{input_file}`", VERBOSE_STRING.as_str());
            }
            let input = std::fs::read_to_string(&input_file)?;

            let mut tokenizer = Tokenizer::new(input);
            let tokens = tokenizer.tokenize().unwrap();
            let mut parser = frontend::parser::Parser::new(tokens);
            let mut statements = parser.parse()?;

            let mut checker = Checker::new(&mut statements);
            checker.check()?;

            let context = Context::new();
            let mut converter = ApolloBuilder::new_with(&context, &mut checker, &input_file)?;
            converter.start();
            converter.evaluate_all()?;
            converter.end()?;
            converter.set_triple(&target_triple);
            if verbose {
                eprintln!("{} Creating directory `./tmp`", VERBOSE_STRING.as_str());
            }
            std::fs::create_dir_all("./tmp")?;
            if verbose {
                eprintln!("{} Writing output bitcode to `./tmp/main.bc`", VERBOSE_STRING.as_str());
            }
            std::fs::write("./tmp/main.bc", converter.get_bitcode()?)?;
            match output_type.unwrap_or("executable".to_string()).as_str() {
                "executable" | "exec" | "binary" | "bin" => {
                    if verbose {
                        eprintln!("{} Executing command `clang++ ./tmp/main.bc -o {output_file} -O{optimization} -Wno-override-module`", VERBOSE_STRING.as_str());
                    }
                    let process = std::process::Command::new("clang++")
                        .args([
                            "./tmp/main.bc",
                            "-o",
                            &output_file,
                            &format!("-O{optimization}"),
                            "-Wno-override-module",
                        ])
                        .spawn()?
                        .wait()?;
                    if process.success() {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Ok(())
                    } else if let Some(code) = process.code() {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!(format!(
                            "clang++ exited with non-zero exit code: {code}"
                        )))
                    } else {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!("clang++ was terminated by a signal"))
                    }
                }
                "object" | "obj" | "o" => {
                    if verbose {
                        eprintln!("{} Executing command `clang++ ./tmp/main.bc -o {output_file} -O{optimization} -c`", VERBOSE_STRING.as_str());
                    }
                    let process = std::process::Command::new("clang++")
                        .args([
                            "./tmp/main.bc",
                            "-o",
                            &output_file,
                            &format!("-O{optimization}"),
                            "-c",
                        ])
                        .spawn()?
                        .wait()?;
                    if process.success() {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Ok(())
                    } else if let Some(code) = process.code() {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!(format!(
                            "clang++ exited with non-zero exit code: {code}"
                        )))
                    } else {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!("clang++ was terminated by a signal"))
                    }
                }
                "llvm-ir" | "ir" => {
                    if verbose {
                        eprintln!("{} Executing command `opt ./tmp/main.bc -o ./tmp/opt.bc -O{optimization}`", VERBOSE_STRING.as_str());
                    }
                    let process = std::process::Command::new("opt")
                        .args(["./tmp/main.bc", "-o", "./tmp/opt.bc", &format!("-O{optimization}")])
                        .spawn()?
                        .wait()?;
                    if process.success() {
                        if verbose {
                            eprintln!("{} Executing command `llvm-dis ./tmp/opt.bc -o {output_file}`", VERBOSE_STRING.as_str());
                        }
                        let process = std::process::Command::new("llvm-dis")
                            .args(["./tmp/opt.bc", "-o", &output_file])
                            .spawn()?
                            .wait()?;
                        if process.success() {
                            if verbose {
                                eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                            }
                            if !noclear {
                                let _ = std::fs::remove_dir_all("./tmp");
                            }
                            Ok(())
                        } else if let Some(code) = process.code() {
                            if verbose {
                                eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                            }
                            if !noclear {
                                let _ = std::fs::remove_dir_all("./tmp");
                            }
                            Err(anyhow!(format!(
                                "llvm-dis exited with non-zero exit code: {code}"
                            )))
                        } else {
                            if verbose {
                                eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                            }
                            if !noclear {
                                let _ = std::fs::remove_dir_all("./tmp");
                            }
                            Err(anyhow!("llvm-dis was terminated by a signal"))
                        }
                    } else if let Some(code) = process.code() {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!(format!(
                            "opt exited with non-zero exit code: {code}"
                        )))
                    } else {
                        if verbose {
                            eprintln!("{} Removing directory `./tmp`", VERBOSE_STRING.as_str());
                        }
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!("opt was terminated by a signal"))
                    }
                }
                "assembly" | "asm" | "s" => {
                    if verbose {
                        eprintln!("{} Executing command `llc ./tmp/main.bc -o {output_file} -O{optimization}`", VERBOSE_STRING.as_str());
                    }
                    let process = std::process::Command::new("llc")
                        .args(["./tmp/main.bc", "-o", &output_file, &format!("-O{optimization}")])
                        .spawn()?
                        .wait()?;
                    if process.success() {
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Ok(())
                    } else if let Some(code) = process.code() {
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!(format!(
                            "llc exited with non-zero exit code: {code}"
                        )))
                    } else {
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!("llc was terminated by a signal"))
                    }
                }
                "llvm-bitcode" | "bitcode" | "bc" => {
                    let process = std::process::Command::new("mv")
                        .args(["./tmp/main.bc", &output_file])
                        .spawn()?
                        .wait()?;
                    if process.success() {
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Ok(())
                    } else if let Some(code) = process.code() {
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!(format!(
                            "mv exited with non-zero exit code: {code}"
                        )))
                    } else {
                        if !noclear {
                            let _ = std::fs::remove_dir_all("./tmp");
                        }
                        Err(anyhow!("mv was terminated by a signal"))
                    }
                }
                output_type => {
                    eprintln!(
                        r#"Available output file types:
    Executable:   executable | binary | exec | bin
    Object:       object | obj | o
    LLVM IR:      llvm-ir | ir
    Assembly:     assembly | asm | s
    LLVM Bitcode: llvm-bitcode | bitcode | bc"#
                    );
                    Err(anyhow!("Unknown output file type: {output_type}"))
                }
            }
        }
        Subcommands::Print { what_to_print } => match what_to_print.as_str() {
            "calling-conventions" => {
eprintln!("Calling conventions:
    C, FastCall, Cold, GHC, HiPE, WebKitJS, AnyReg, PreserveMost, PreserveAll, Swift, CXXFastTLS, Tail, CFGuardCheck, SwiftTail, X86StdCall, X86FastCall, ARMAPCS, ARMAAPCS, ARMAAPCSVFP, MSP430Intr, X86ThisCall, PTXKernel, PTXDevice, SPIRFunc, SPIRKernel, IntelOCLBI, SystemV, Windows, X86VectorCall, DummyHHVM, DummyHHVMC, X86Intr, AVRIntr, AVRSignal, AVRBuiltin, AMDGPUVS, AMDGPUGS, AMDGPUPS, AMDGPUCS, AMDGPUKernel, X86RegCall, AMDGPUHS, MSP430Builtin, AMDGPULS, AMDGPUES, AArch64VectorCall, AArch64SVEVectorCall, WASMEmscriptenInvoke, AMDGPUGfx, M68kIntr, AArch64SMEABI_Support_Routines_PreserveMost_From_X0, AArch64SMEABI_Support_Routines_PreserveMost_From_X2, AMDGPUCSChain, AMDGPUCSChainPreserve and MaxID");
                Ok(())
            }
            _ => {
                Err(anyhow!("Cannot print `{what_to_print}`"))
            }
        }
    }
}

extern crate backtrace;

fn generate_stack_dump(state: &'static str) {
    let bt = backtrace::Backtrace::new();

    // print the current stack trace
    println!("Stack dump:");
    for (i, frame) in bt.frames().iter().enumerate() {
        let ip = frame.ip();
        let symbols = frame.symbols();
        if symbols.is_empty() {
            println!("{}. <unknown>:<unknown>:<unknown>", i);
        } else {
            for symbol in symbols {
                let name = symbol.name().map(|name| format!("{}", name)).unwrap_or("<unknown>".to_string());
                let filename = symbol.filename().map(|filename| format!("{}", filename.display())).unwrap_or("<unknown>".to_string());
                let lineno = symbol.lineno().map(|lineno| format!("{}", lineno)).unwrap_or("<unknown>".to_string());
                println!("#{}. {}: {} ({})", Color::White.paint(i.to_string()), Color::Yellow.paint(lineno), Color::RGB(95, 255, 245).paint(name), Color::Green.paint(filename));
            }
        }
    }
    println!("{}", Color::White.bold().paint(state));
}
