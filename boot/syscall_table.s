/*
i960SxChipset
Copyright (c) 2026, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

# the system procedure table will _only_ be used if the user make a supervisor procedure call
.include "macros.s"
    .align 6
.global sys_proc_table
sys_proc_table:
    .word 0 # Reserved
    .word 0 # Reserved
    .word 0 # Reserved
    .word (_sup_stack + 0x1) # Supervisor stack pointer
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
# up to 260 entries!
    # example entry
	ReservedTableEntry # 0
	ReservedTableEntry # 1
	ReservedTableEntry # 2
	ReservedTableEntry # 3
	ReservedTableEntry # 4
	ReservedTableEntry # 5
	ReservedTableEntry # 6
	ReservedTableEntry # 7
	ReservedTableEntry # 8
	ReservedTableEntry # 9
	ReservedTableEntry # 10
	ReservedTableEntry # 11
	ReservedTableEntry # 12
	ReservedTableEntry # 13
	ReservedTableEntry # 14
	ReservedTableEntry # 15
	ReservedTableEntry # 16
	ReservedTableEntry # 17
	ReservedTableEntry # 18
	ReservedTableEntry # 19
	ReservedTableEntry # 20
	ReservedTableEntry # 21
	ReservedTableEntry # 22
	ReservedTableEntry # 23
    ReservedTableEntry # 24
    ReservedTableEntry # 25
    ReservedTableEntry # 26
    ReservedTableEntry # 27
    ReservedTableEntry # 28
    ReservedTableEntry # 29
    ReservedTableEntry # 30
    ReservedTableEntry # 31
    ReservedTableEntry # 32
    ReservedTableEntry # 33
    ReservedTableEntry # 34
    ReservedTableEntry # 35
    ReservedTableEntry # 36
    ReservedTableEntry # 37
    ReservedTableEntry # 38
    ReservedTableEntry # 39
    ReservedTableEntry # 40
    ReservedTableEntry # 41
    ReservedTableEntry # 42
    ReservedTableEntry # 43
    ReservedTableEntry # 44
    ReservedTableEntry # 45
    ReservedTableEntry # 46
    ReservedTableEntry # 47
    ReservedTableEntry # 48
    ReservedTableEntry # 49
    ReservedTableEntry # 50
    ReservedTableEntry # 51
    ReservedTableEntry # 52
    ReservedTableEntry # 53
    ReservedTableEntry # 54
    ReservedTableEntry # 55
    ReservedTableEntry # 56
    ReservedTableEntry # 57
    ReservedTableEntry # 58
    ReservedTableEntry # 59
    ReservedTableEntry # 60
    ReservedTableEntry # 61
    ReservedTableEntry # 62
    ReservedTableEntry # 63
    ReservedTableEntry # 64
    ReservedTableEntry # 65
    ReservedTableEntry # 66
    ReservedTableEntry # 67
    ReservedTableEntry # 68
    ReservedTableEntry # 69
    ReservedTableEntry # 70
    ReservedTableEntry # 71
    ReservedTableEntry # 72
    ReservedTableEntry # 73
    ReservedTableEntry # 74
    ReservedTableEntry # 75
    ReservedTableEntry # 76
    ReservedTableEntry # 77
    ReservedTableEntry # 78
    ReservedTableEntry # 79
    ReservedTableEntry # 80
    ReservedTableEntry # 81
    ReservedTableEntry # 82
    ReservedTableEntry # 83
    ReservedTableEntry # 84
    ReservedTableEntry # 85
    ReservedTableEntry # 86
    ReservedTableEntry # 87
    ReservedTableEntry # 88
    ReservedTableEntry # 89
    ReservedTableEntry # 90
    ReservedTableEntry # 91
    ReservedTableEntry # 92
    ReservedTableEntry # 93
    ReservedTableEntry # 94
    ReservedTableEntry # 95
    ReservedTableEntry # 96
    ReservedTableEntry # 97
    ReservedTableEntry # 98
    ReservedTableEntry # 99
    ReservedTableEntry # 100
    ReservedTableEntry # 101
    ReservedTableEntry # 102
    ReservedTableEntry # 103
    ReservedTableEntry # 104
    ReservedTableEntry # 105
    ReservedTableEntry # 106
    ReservedTableEntry # 107
    ReservedTableEntry # 108
    ReservedTableEntry # 109
    ReservedTableEntry # 110
    ReservedTableEntry # 111
    ReservedTableEntry # 112
    ReservedTableEntry # 113
    ReservedTableEntry # 114
    ReservedTableEntry # 115
    ReservedTableEntry # 116
    ReservedTableEntry # 117
    ReservedTableEntry # 118
    ReservedTableEntry # 119
    ReservedTableEntry # 120
    ReservedTableEntry # 121
    ReservedTableEntry # 122
    ReservedTableEntry # 123
    ReservedTableEntry # 124
    ReservedTableEntry # 125
    ReservedTableEntry # 126
    ReservedTableEntry # 127
    ReservedTableEntry # 128
    ReservedTableEntry # 129
    ReservedTableEntry # 130
    ReservedTableEntry # 131
    ReservedTableEntry # 132
    ReservedTableEntry # 133
    ReservedTableEntry # 134
    ReservedTableEntry # 135
    ReservedTableEntry # 136
    ReservedTableEntry # 137
    ReservedTableEntry # 138
    ReservedTableEntry # 139
    ReservedTableEntry # 140
    ReservedTableEntry # 141
    ReservedTableEntry # 142
    ReservedTableEntry # 143
    ReservedTableEntry # 144
    ReservedTableEntry # 145
    ReservedTableEntry # 146
    ReservedTableEntry # 147
    ReservedTableEntry # 148
    ReservedTableEntry # 149
    ReservedTableEntry # 150
    ReservedTableEntry # 151
    ReservedTableEntry # 152
    ReservedTableEntry # 153
    ReservedTableEntry # 154
    ReservedTableEntry # 155
    ReservedTableEntry # 156
    ReservedTableEntry # 157
    ReservedTableEntry # 158
    ReservedTableEntry # 159
    ReservedTableEntry # 160
    ReservedTableEntry # 161
    ReservedTableEntry # 162
    ReservedTableEntry # 163
    ReservedTableEntry # 164
    ReservedTableEntry # 165
    ReservedTableEntry # 166
    ReservedTableEntry # 167
    ReservedTableEntry # 168
    ReservedTableEntry # 169
    ReservedTableEntry # 170
    ReservedTableEntry # 171
    ReservedTableEntry # 172
    ReservedTableEntry # 173
    ReservedTableEntry # 174
    ReservedTableEntry # 175
    ReservedTableEntry # 176
    ReservedTableEntry # 177
    ReservedTableEntry # 178
    ReservedTableEntry # 179
    ReservedTableEntry # 180
    ReservedTableEntry # 181
    ReservedTableEntry # 182
    ReservedTableEntry # 183
    ReservedTableEntry # 184
    ReservedTableEntry # 185
    ReservedTableEntry # 186
    ReservedTableEntry # 187
    ReservedTableEntry # 188
    ReservedTableEntry # 189
    ReservedTableEntry # 190
    ReservedTableEntry # 191
    ReservedTableEntry # 192
    ReservedTableEntry # 193
    ReservedTableEntry # 194
    ReservedTableEntry # 195
    ReservedTableEntry # 196
    ReservedTableEntry # 197
    ReservedTableEntry # 198
    ReservedTableEntry # 199
    ReservedTableEntry # 200
    ReservedTableEntry # 201
    ReservedTableEntry # 202
    ReservedTableEntry # 203
    ReservedTableEntry # 204
    ReservedTableEntry # 205
    ReservedTableEntry # 206
    ReservedTableEntry # 207
    ReservedTableEntry # 208
    ReservedTableEntry # 209
    ReservedTableEntry # 210
    ReservedTableEntry # 211
    ReservedTableEntry # 212
    ReservedTableEntry # 213
    ReservedTableEntry # 214
    ReservedTableEntry # 215
    ReservedTableEntry # 216
    ReservedTableEntry # 217
    ReservedTableEntry # 218
    ReservedTableEntry # 219
    ReservedTableEntry # 220
    ReservedTableEntry # 221
    ReservedTableEntry # 222
    ReservedTableEntry # 223
    ReservedTableEntry # 224
    ReservedTableEntry # 225
    ReservedTableEntry # 226
    ReservedTableEntry # 227
	ReservedTableEntry  # 228
	ReservedTableEntry  # 229
	ReservedTableEntry  # 230
	ReservedTableEntry  # 231
	ReservedTableEntry  # 232
	ReservedTableEntry  # 233
	ReservedTableEntry  # 234
	ReservedTableEntry # 235
	ReservedTableEntry # 236
	ReservedTableEntry # 237
	ReservedTableEntry # 238
	ReservedTableEntry # 239
	ReservedTableEntry # 240
	ReservedTableEntry # 241
	ReservedTableEntry # 242
# libc routines
	DefTableEntry hitagi_access # 243
	DefTableEntry hitagi_link # 244
	DefTableEntry hitagi_isatty # 245
	DefTableEntry hitagi_setitimer # 246
	DefTableEntry hitagi_gettimeofday # 247
    DefTableEntry hitagi_getrusage # 248
	DefTableEntry hitagi_sbrk # 249
	DefTableEntry hitagi_fstat # 250
	DefTableEntry hitagi_getpid # 251
	DefTableEntry hitagi_unlink # 252
    DefTableEntry hitagi_kill # 253
	DefTableEntry hitagi_open # 254
	DefTableEntry hitagi_read # 255
	DefTableEntry hitagi_write # 256
	DefTableEntry hitagi_lseek # 257
	DefTableEntry hitagi_close # 258
	DefTableEntry hitagi_exit # 259
# up to a total of 260 entries
# reserved entries
#def_system_call 12, _sys_argvlen
#def_system_call 13, _sys_argv
#def_system_call 14, _sys_chdir
#def_system_call 15, _sys_stat
#def_system_call 16, _sys_chmod
#def_system_call 17, _sys_utime
#def_system_call 18, _sys_time
def_system_call 243, _sys_access
def_system_call 244, _sys_link
def_system_call 245, _sys_isatty
def_system_call 246, _sys_setitimer
def_system_call 247, _sys_gettimeofday
def_system_call 248, _sys_getrusage
def_system_call 249, _sys_sbrk
def_system_call 250, _sys_fstat
def_system_call 251, _sys_getpid
def_system_call 252, _sys_unlink
def_system_call 253, _sys_kill
def_system_call 254, _sys_open
def_system_call 255, _sys_read
def_system_call 256, _sys_write
def_system_call 257, _sys_lseek
def_system_call 258, _sys_close
def_system_call 259, _exit
