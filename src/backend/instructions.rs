enum Instruction {
    // extend instructions
    /// extends an 8 bit value to a 16 bit value
    /// 
    /// **Syntax**: ext8t16 %*register*, %*register*
    Ext8T16(Register, Register),
    /// extends an 8 bit value to a 32 bit value
    /// 
    /// **Syntax**: ext8t32 %*register*, %*register*
    Ext8T32(Register, Register),
    /// extends an 8 bit value to a 64 bit value
    /// 
    /// **Syntax**: ext8t64 %*register*, %*register*
    Ext8T64(Register, Register),
    /// extends an 16 bit value to a 32 bit value
    /// 
    /// **Syntax**: ext16t32 %*register*, %*register*
    Ext16T32(Register, Register),
    /// extends an 16 bit value to a 64 bit value
    /// 
    /// **Syntax**: ext16t64 %*register*, %*register*
    Ext16T64(Register, Register),
    /// extends an 32 bit value to a 64 bit value
    /// 
    /// **Syntax**: ext32t64 %*register*, %*register*
    Ext32T64(Register, Register),

    // reduce instructions
    /// narrows an 64 bit value to a 32 bit value
    /// 
    /// **Syntax**: nar64t32 %*register*, %*register*
    Nar64T32(Register, Register),
    /// narrows an 64 bit value to a 16 bit value
    /// 
    /// **Syntax**: nar64t16 %*register*, %*register*
    Nar64T16(Register, Register),
    /// narrows an 64 bit value to a 8 bit value
    /// 
    /// **Syntax**: nar64t8 %*register*, %*register*
    Nar64T8(Register, Register),
    /// narrows an 32 bit value to a 16 bit value
    /// 
    /// **Syntax**: nar32t16 %*register*, %*register*
    Nar32T16(Register, Register),
    /// narrows an 32 bit value to a 8 bit value
    /// 
    /// **Syntax**: nar32t8 %*register*, %*register*
    Nar32T8(Register, Register),
    /// narrows an 16 bit value to a 8 bit value
    /// 
    /// **Syntax**: nar16t8 %*register*, %*register*
    Nar16T8(Register, Register),

    // load address of instruction
    /// 
    /// **Syntax**: laof *location*, %*register*
    Laof(String, Register),

    // add instructions
    /// adds two unsigned integers
    /// 
    /// **Syntax**: addui %*register*, %*register*
    AddUI(Register, Register), 
    /// adds two signed integers
    /// 
    /// **Syntax**: addsi %*register*, %*register*
    AddSI(Register, Register), 
    /// adds two doubles
    /// 
    /// **Syntax**: adddb %*register*, %*register*
    AddDB(Register, Register), 
    /// adds two floats
    /// 
    /// **Syntax**: addft %*register*, %*register*
    AddFT(Register, Register), 
    
    // sub instructions
    /// subtracts two unsigned integers
    /// 
    /// **Syntax**: subui %*register*, %*register*
    SubUI(Register, Register), 
    /// subtracts two signed integers
    /// 
    /// **Syntax**: subsi %*register*, %*register*
    SubSI(Register, Register), 
    /// subtracts two doubles
    /// 
    /// **Syntax**: subdb %*register*, %*register*
    SubDB(Register, Register), 
    /// subtracts two floats
    /// 
    /// **Syntax**: subft %*register*, %*register*
    SubFT(Register, Register), 
    
    // mul instructions
    /// multiplies two unsigned integers
    /// 
    /// **Syntax**: mului %*register*, %*register*
    MulUI(Register, Register), 
    /// multiplies two signed integers
    /// 
    /// **Syntax**: mulsi %*register*, %*register*
    MulSI(Register, Register), 
    /// multiplies two doubles
    /// 
    /// **Syntax**: muldb %*register*, %*register*
    MulDB(Register, Register), 
    /// multiplies two floats
    /// 
    /// **Syntax**: mulft %*register*, %*register*
    MulFT(Register, Register), 
    
    // div instructions
    /// divides two unsigned integers
    /// 
    /// **Syntax**: divui %*register*, %*register*
    DivUI(Register, Register),
    /// divides two signed integers
    /// 
    /// **Syntax**: divsi %*register*, %*register*
    DivSI(Register, Register), 
    /// divides two doubles
    /// 
    /// **Syntax**: divdb %*register*, %*register*
    DivDB(Register, Register), 
    /// divides two floats
    /// 
    /// **Syntax**: divft %*register*, %*register*
    DivFT(Register, Register), 
    
    // rem instructions
    /// applies the remainder operator to two unsigned integers
    /// 
    /// **Syntax**: remui %*register*, %*register*
    RemUI(Register, Register),
    /// applies the remainder operator to two signed integers
    /// 
    /// **Syntax**: remsi %*register*, %*register*
    RemSI(Register, Register),
    /// applies the remainder operator to two doubles
    /// 
    /// **Syntax**: remdb %*register*, %*register*
    RemDB(Register, Register), 
    /// applies the remainder operator to two floats
    /// 
    /// **Syntax**: remft %*register*, %*register*
    RemFT(Register, Register), 
    
    // bitwise instructions
    /// ands two operands
    /// 
    /// **Syntax**: and %*register*, %*register*
    And(Register, Register), 
    /// ors two operands
    /// 
    /// **Syntax**: or %*register*, %*register*
    Or(Register, Register), 
    /// xors two operands
    /// 
    /// **Syntax**: xor %*register*, %*register*
    Xor(Register, Register), 
    /// negates two operands
    /// 
    /// **Syntax**: not %*register*
    Not(Register), 

    /// shifts to left two operands
    /// 
    /// **Syntax**: shl %*register*, %*register*
    Shl(Register, Register), 
    /// shifts to right two operands
    /// 
    /// **Syntax**: shr %*register*, %*register*
    Shr(Register, Register), 

    /// Makes a register zero.
    /// 
    /// **Syntax**: zero %*register*
    Zero(Register),

    /// move an immediate value into a register
    /// 
    /// **Syntax**: movi %*dst*, $*immediate*
    MoveImmediate(Register, u64),

    /// move values between registers
    /// 
    /// **Syntax**: movr %*dst*, %*src*
    MoveRegister(Register, Register),

    /// move from an address to a register
    /// 
    /// **Syntax**: movf %*register*, *address*
    MoveFrom(Register, String),

    /// move from a register to an address
    /// 
    /// **Syntax**: movt *address*, %*register*
    MoveTo(String, Register),

    /// jumps to the body of a function and pushes the return address
    /// to the stack as a 64-bit integer.
    /// 
    /// **Syntax**: call *function*
    Call(Register, String),

    /// pops a return address from the stack and jumps to it.
    /// 
    /// **Syntax**: ret
    Ret,

    /// jumps to a label
    /// 
    /// **Syntax**: jl *label*
    JumpToLabel(String),

    /// jumps to a label if the value stored in the register is equal to all ones.
    /// 
    /// **Syntax**: jlc *label, %*register*
    JumpToLabelConditionally(String, Register),

    /// generates a dump of the current stack state and prints it.
    /// this also halts the program execution.
    /// 
    /// **Syntax**: dmp
    Dump,

    /// sets a landing pad to the current stack frame with the wide mode enabled.
    /// this takes the register in which to store the exception object.
    /// it also takes a label to jump to if an exception is found and one to jump to if everything is okay.
    /// 
    /// **Syntax**: lp %*register*, *okay_label*, *exception_label*
    LandingPad(Register, String, String),

    /// unwinds the stack trying to find a landing pad that is either wide or has the same code as this.
    /// this also takes in the register that has the exception's object id.
    /// 
    /// **Syntax**: uwd $*code*, %*register*
    Unwind(Register),
}

enum Register {
    // 64-bit registers
    A64,
    B64,
    C64,
    D64,
    E64,

    // 32-bit registers
    A32,
    B32,
    C32,
    D32,
    E32,

    // 16-bit registers
    A16,
    B16,
    C16,
    D16,
    E16,

    // 8-bit registers
    A8,
    B8,
    C8,
    D8,
    E8,

    // IP register
    /// always stores the current instruction pointer
    InstructionPointer,

    // PA register
    /// always stores the previous instruction pointer
    PreviousAddress,
}
