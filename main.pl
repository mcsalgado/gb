:- use_module(library(clpfd)).

opcode(Z, Q, P, X) :-
    Z in 0..7,
    Q in 0..1,
    P in 0..3,
    X in 0..3.
opcode(Z, Y, X) :-
    Z in 0..7,
    Y in 0..7,
    X in 0..3.

opcode(Z, Y, X) :- opcode(Z, Y mod 2, Y >> 1, X).


register(b, 0).
register(c, 1).
register(d, 2).
register(e, 3).
register(h, 4).
register(l, 5).
register(p_hl_p, 6).
register(a, 7).

register_pair(bc, 0).
register_pair(de, 1).
register_pair(hl, 2).
register_pair(sp, 3).

register_pair2(bc, 0).
register_pair2(de, 1).
register_pair2(hl, 2).
register_pair2(af, 3).

condition(nz, 0).
condition(z, 1).
condition(nc, 2).
condition(c, 3).


encode(instruction(nop), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(0, 0, 0)).

encode(instruction(inc, Operand), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(3, 0, RegisterPairIndex, 0)),
    register_pair(Operand, RegisterPairIndex).

encode(instruction(dec, Operand), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(3, 1, RegisterPairIndex, 0)),
    register_pair(Operand, RegisterPairIndex).

encode(instruction(inc, Operand), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(4, RegisterIndex, 0)),
    register(Operand, RegisterIndex).

encode(instruction(dec, Operand), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(5, RegisterIndex, 0)),
    register(Operand, RegisterIndex).


% NOTE(mcsalgado): assorted operations on accumulator/flags

encode(instruction(rlca), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 0, 0)).

encode(instruction(rrca), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 1, 0)).

encode(instruction(rla), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 2, 0)).

encode(instruction(rra), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 3, 0)).

encode(instruction(daa), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 4, 0)).

encode(instruction(cpl), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 5, 0)).

encode(instruction(scf), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 6, 0)).

encode(instruction(ccf), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(7, 7, 0)).


encode(instruction(halt), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(6, 6, 1)).

encode(instruction(ld, Dest, Src), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(SrcRegisterIndex, DestRegisterIndex, 1)),
    register(Dest, DestRegisterIndex),
    register(Src, SrcRegisterIndex).

encode(instruction(ld, Operand, N), MachineInstruction) :-
    N in 0..255,
    MachineInstruction = machine_instruction(opcode(6, RegisterIndex, 0), ImmediateData),
    register(Operand, RegisterIndex),
    ImmediateData = N.

% NOTE(mcsalgado): conditional return
encode(instruction(ret, Operand), MachineInstruction) :-
    MachineInstruction = opcode(0, ConditionIndex, 3),
    condition(Operand, ConditionIndex).

encode(instruction(pop, Operand), MachineInstruction) :-
    MachineInstruction = opcode(1, 0, RegisterPair2Index, 3),
    register_pair2(Operand, RegisterPair2Index).

encode(instruction(di), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(3, 6, 3)).

encode(instruction(ei), MachineInstruction) :-
    MachineInstruction = machine_instruction(opcode(3, 7, 3)).

encode(instruction(push, Operand), MachineInstruction) :-
    MachineInstruction = opcode(5, 0, RegisterPair2Index, 3),
    register_pair2(Operand, RegisterPair2Index).

encode(instruction(rst, Operand), MachineInstruction) :-
    MachineInstruction = opcode(7, Y, 3),
    Operand #= Y*8.

encode_list([], []).
encode_list([H | T], [H1 | T1]) :-
    encode(H, H1),
    encode_list(T, T1).


assemble(machine_instruction(opcode(Z, Q, P, X)), Assembled) :-
    Assembled in 0..255,
%   Assembled #= (Z + (Q << 3) + (P << 4) + (X << 6)).
    Assembled #= (Z + (Q * 8) + (P * 16) + (X * 64)).

assemble(machine_instruction(opcode(Z, Y, X)), Assembled) :-
    Assembled in 0..255,
    Assembled #= (Z + (Y * 8) + (X * 64)).

assemble_immediate(machine_instruction(Opcode, ImmediateData), Assembled) :-
    X in 0..255,
    ImmediateData in 0..255,
    assemble(machine_instruction(Opcode), X),
    Assembled = [X, ImmediateData].

assemble_list([], []).
assemble_list([H | T], [H11, H12 | T1]) :-
    assemble_immediate(H, [H11, H12]),
    assemble_list(T, T1).
assemble_list([H | T], [H1 | T1]) :-
    assemble(H, H1),
    assemble_list(T, T1).


compile_list(Instructions, AssembledList) :-
    encode_list(Instructions, MachineInstructions),
    assemble_list(MachineInstructions, AssembledList).

compile_list2(Instructions, AssembledList) :-
    assemble_list(MachineInstructions, AssembledList),
    encode_list(Instructions, MachineInstructions).
