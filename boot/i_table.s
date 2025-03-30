/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
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
/* Initial interrupt table
 * in my board, there is only one interrupt routine so they will all map to the same ISR
 */

        .global     intr_table
        .align      6
intr_table:
        .word       0               # Pending Priorities    0
        .fill       8, 4, 0         # Pending Interrupts    4 + (0->7)*4
        .word do_nothing_isr           # interrupt table entry 8
        .word do_nothing_isr           # interrupt table entry 9
        .word do_nothing_isr           # interrupt table entry 10
        .word do_nothing_isr           # interrupt table entry 11
        .word do_nothing_isr           # interrupt table entry 12
        .word do_nothing_isr           # interrupt table entry 13
        .word do_nothing_isr           # interrupt table entry 14
        .word do_nothing_isr           # interrupt table entry 15
        .word do_nothing_isr           # interrupt table entry 16
        .word do_nothing_isr           # interrupt table entry 17
        .word do_nothing_isr           # interrupt table entry 18
        .word do_nothing_isr           # interrupt table entry 19
        .word do_nothing_isr           # interrupt table entry 20
        .word do_nothing_isr           # interrupt table entry 21
        .word do_nothing_isr           # interrupt table entry 22
        .word do_nothing_isr           # interrupt table entry 23
        .word do_nothing_isr           # interrupt table entry 24
        .word do_nothing_isr           # interrupt table entry 25
        .word do_nothing_isr           # interrupt table entry 26
        .word do_nothing_isr           # interrupt table entry 27
        .word do_nothing_isr           # interrupt table entry 28
        .word do_nothing_isr           # interrupt table entry 29
        .word do_nothing_isr           # interrupt table entry 30
        .word do_nothing_isr           # interrupt table entry 31
        .word do_nothing_isr           # interrupt table entry 32
        .word do_nothing_isr           # interrupt table entry 33
        .word do_nothing_isr           # interrupt table entry 34
        .word do_nothing_isr           # interrupt table entry 35
        .word do_nothing_isr           # interrupt table entry 36
        .word do_nothing_isr           # interrupt table entry 37
        .word do_nothing_isr           # interrupt table entry 38
        .word do_nothing_isr           # interrupt table entry 39
        .word do_nothing_isr           # interrupt table entry 40
        .word do_nothing_isr           # interrupt table entry 41
        .word do_nothing_isr           # interrupt table entry 42
        .word do_nothing_isr           # interrupt table entry 43
        .word do_nothing_isr           # interrupt table entry 44
        .word do_nothing_isr           # interrupt table entry 45
        .word do_nothing_isr           # interrupt table entry 46
        .word do_nothing_isr           # interrupt table entry 47
        .word do_nothing_isr           # interrupt table entry 48
        .word do_nothing_isr           # interrupt table entry 49
        .word do_nothing_isr           # interrupt table entry 50
        .word do_nothing_isr           # interrupt table entry 51
        .word do_nothing_isr           # interrupt table entry 52
        .word do_nothing_isr           # interrupt table entry 53
        .word do_nothing_isr           # interrupt table entry 54
        .word do_nothing_isr           # interrupt table entry 55
        .word do_nothing_isr           # interrupt table entry 56
        .word do_nothing_isr           # interrupt table entry 57
        .word do_nothing_isr           # interrupt table entry 58
        .word do_nothing_isr           # interrupt table entry 59
        .word do_nothing_isr           # interrupt table entry 60
        .word do_nothing_isr           # interrupt table entry 61
        .word do_nothing_isr           # interrupt table entry 62
        .word do_nothing_isr           # interrupt table entry 63
        .word do_nothing_isr           # interrupt table entry 64
        .word do_nothing_isr           # interrupt table entry 65
        .word do_nothing_isr           # interrupt table entry 66
        .word do_nothing_isr           # interrupt table entry 67
        .word do_nothing_isr           # interrupt table entry 68
        .word do_nothing_isr           # interrupt table entry 69
        .word do_nothing_isr           # interrupt table entry 70
        .word do_nothing_isr           # interrupt table entry 71
        .word do_nothing_isr           # interrupt table entry 72
        .word do_nothing_isr           # interrupt table entry 73
        .word do_nothing_isr           # interrupt table entry 74
        .word do_nothing_isr           # interrupt table entry 75
        .word do_nothing_isr           # interrupt table entry 76
        .word do_nothing_isr           # interrupt table entry 77
        .word do_nothing_isr           # interrupt table entry 78
        .word do_nothing_isr           # interrupt table entry 79
        .word do_nothing_isr           # interrupt table entry 80
        .word do_nothing_isr           # interrupt table entry 81
        .word do_nothing_isr           # interrupt table entry 82
        .word do_nothing_isr           # interrupt table entry 83
        .word do_nothing_isr           # interrupt table entry 84
        .word do_nothing_isr           # interrupt table entry 85
        .word do_nothing_isr           # interrupt table entry 86
        .word do_nothing_isr           # interrupt table entry 87
        .word do_nothing_isr           # interrupt table entry 88
        .word do_nothing_isr           # interrupt table entry 89
        .word do_nothing_isr           # interrupt table entry 90
        .word do_nothing_isr           # interrupt table entry 91
        .word do_nothing_isr           # interrupt table entry 92
        .word do_nothing_isr           # interrupt table entry 93
        .word do_nothing_isr           # interrupt table entry 94
        .word do_nothing_isr           # interrupt table entry 95
        .word do_nothing_isr           # interrupt table entry 96
        .word do_nothing_isr           # interrupt table entry 97
        .word do_nothing_isr           # interrupt table entry 98
        .word do_nothing_isr           # interrupt table entry 99
        .word do_nothing_isr           # interrupt table entry 100
        .word do_nothing_isr           # interrupt table entry 101
        .word do_nothing_isr           # interrupt table entry 102
        .word do_nothing_isr           # interrupt table entry 103
        .word do_nothing_isr           # interrupt table entry 104
        .word do_nothing_isr           # interrupt table entry 105
        .word do_nothing_isr           # interrupt table entry 106
        .word do_nothing_isr           # interrupt table entry 107
        .word do_nothing_isr           # interrupt table entry 108
        .word do_nothing_isr           # interrupt table entry 109
        .word do_nothing_isr           # interrupt table entry 110
        .word do_nothing_isr           # interrupt table entry 111
        .word do_nothing_isr           # interrupt table entry 112
        .word do_nothing_isr           # interrupt table entry 113
        .word do_nothing_isr           # interrupt table entry 114
        .word do_nothing_isr           # interrupt table entry 115
        .word do_nothing_isr           # interrupt table entry 116
        .word do_nothing_isr           # interrupt table entry 117
        .word do_nothing_isr           # interrupt table entry 118
        .word do_nothing_isr           # interrupt table entry 119
        .word do_nothing_isr           # interrupt table entry 120
        .word do_nothing_isr           # interrupt table entry 121
        .word do_nothing_isr           # interrupt table entry 122
        .word do_nothing_isr           # interrupt table entry 123
        .word do_nothing_isr           # interrupt table entry 124
        .word do_nothing_isr           # interrupt table entry 125
        .word do_nothing_isr           # interrupt table entry 126
        .word do_nothing_isr           # interrupt table entry 127
        .word do_nothing_isr           # interrupt table entry 128
        .word do_nothing_isr           # interrupt table entry 129
        .word do_nothing_isr           # interrupt table entry 130
        .word do_nothing_isr           # interrupt table entry 131
        .word do_nothing_isr           # interrupt table entry 132
        .word do_nothing_isr           # interrupt table entry 133
        .word do_nothing_isr           # interrupt table entry 134
        .word do_nothing_isr           # interrupt table entry 135
        .word do_nothing_isr           # interrupt table entry 136
        .word do_nothing_isr           # interrupt table entry 137
        .word do_nothing_isr           # interrupt table entry 138
        .word do_nothing_isr           # interrupt table entry 139
        .word do_nothing_isr           # interrupt table entry 140
        .word do_nothing_isr           # interrupt table entry 141
        .word do_nothing_isr           # interrupt table entry 142
        .word do_nothing_isr           # interrupt table entry 143
        .word do_nothing_isr           # interrupt table entry 144
        .word do_nothing_isr           # interrupt table entry 145
        .word do_nothing_isr           # interrupt table entry 146
        .word do_nothing_isr           # interrupt table entry 147
        .word do_nothing_isr           # interrupt table entry 148
        .word do_nothing_isr           # interrupt table entry 149
        .word do_nothing_isr           # interrupt table entry 150
        .word do_nothing_isr           # interrupt table entry 151
        .word do_nothing_isr           # interrupt table entry 152
        .word do_nothing_isr           # interrupt table entry 153
        .word do_nothing_isr           # interrupt table entry 154
        .word do_nothing_isr           # interrupt table entry 155
        .word do_nothing_isr           # interrupt table entry 156
        .word do_nothing_isr           # interrupt table entry 157
        .word do_nothing_isr           # interrupt table entry 158
        .word do_nothing_isr           # interrupt table entry 159
        .word do_nothing_isr           # interrupt table entry 160
        .word do_nothing_isr           # interrupt table entry 161
        .word do_nothing_isr           # interrupt table entry 162
        .word do_nothing_isr           # interrupt table entry 163
        .word do_nothing_isr           # interrupt table entry 164
        .word do_nothing_isr           # interrupt table entry 165
        .word do_nothing_isr           # interrupt table entry 166
        .word do_nothing_isr           # interrupt table entry 167
        .word do_nothing_isr           # interrupt table entry 168
        .word do_nothing_isr           # interrupt table entry 169
        .word do_nothing_isr           # interrupt table entry 170
        .word do_nothing_isr           # interrupt table entry 171
        .word do_nothing_isr           # interrupt table entry 172
        .word do_nothing_isr           # interrupt table entry 173
        .word do_nothing_isr           # interrupt table entry 174
        .word do_nothing_isr           # interrupt table entry 175
        .word do_nothing_isr           # interrupt table entry 176
        .word do_nothing_isr           # interrupt table entry 177
        .word do_nothing_isr           # interrupt table entry 178
        .word do_nothing_isr           # interrupt table entry 179
        .word do_nothing_isr           # interrupt table entry 180
        .word do_nothing_isr           # interrupt table entry 181
        .word do_nothing_isr           # interrupt table entry 182
        .word do_nothing_isr           # interrupt table entry 183
        .word do_nothing_isr           # interrupt table entry 184
        .word do_nothing_isr           # interrupt table entry 185
        .word do_nothing_isr           # interrupt table entry 186
        .word do_nothing_isr           # interrupt table entry 187
        .word do_nothing_isr           # interrupt table entry 188
        .word do_nothing_isr           # interrupt table entry 189
        .word do_nothing_isr           # interrupt table entry 190
        .word do_nothing_isr           # interrupt table entry 191
        .word do_nothing_isr           # interrupt table entry 192
        .word do_nothing_isr           # interrupt table entry 193
        .word do_nothing_isr           # interrupt table entry 194
        .word do_nothing_isr           # interrupt table entry 195
        .word do_nothing_isr           # interrupt table entry 196
        .word do_nothing_isr           # interrupt table entry 197
        .word do_nothing_isr           # interrupt table entry 198
        .word do_nothing_isr           # interrupt table entry 199
        .word do_nothing_isr           # interrupt table entry 200
        .word do_nothing_isr           # interrupt table entry 201
        .word do_nothing_isr           # interrupt table entry 202
        .word do_nothing_isr           # interrupt table entry 203
        .word do_nothing_isr           # interrupt table entry 204
        .word do_nothing_isr           # interrupt table entry 205
        .word do_nothing_isr           # interrupt table entry 206
        .word do_nothing_isr           # interrupt table entry 207
        .word do_nothing_isr           # interrupt table entry 208
        .word do_nothing_isr           # interrupt table entry 209
        .word do_nothing_isr           # interrupt table entry 210
        .word do_nothing_isr           # interrupt table entry 211
        .word do_nothing_isr           # interrupt table entry 212
        .word do_nothing_isr           # interrupt table entry 213
        .word do_nothing_isr           # interrupt table entry 214
        .word do_nothing_isr           # interrupt table entry 215
        .word do_nothing_isr           # interrupt table entry 216
        .word do_nothing_isr           # interrupt table entry 217
        .word do_nothing_isr           # interrupt table entry 218
        .word do_nothing_isr           # interrupt table entry 219
        .word do_nothing_isr           # interrupt table entry 220
        .word do_nothing_isr           # interrupt table entry 221
        .word do_nothing_isr           # interrupt table entry 222
        .word do_nothing_isr           # interrupt table entry 223

        .word do_nothing_isr           # interrupt table entry 224
        .word do_nothing_isr           # interrupt table entry 225
        .word do_nothing_isr           # interrupt table entry 226
        .word do_nothing_isr           # interrupt table entry 227
        .word do_nothing_isr           # interrupt table entry 228
        .word do_nothing_isr           # interrupt table entry 229
        .word do_nothing_isr           # interrupt table entry 230
        .word do_nothing_isr           # interrupt table entry 231
        .word do_nothing_isr           # interrupt table entry 232
        .word do_nothing_isr           # interrupt table entry 233
        .word do_nothing_isr           # interrupt table entry 234
        .word do_nothing_isr           # interrupt table entry 235
        .word do_nothing_isr           # interrupt table entry 236
        .word do_nothing_isr           # interrupt table entry 237
        .word do_nothing_isr           # interrupt table entry 238
        .word do_nothing_isr           # interrupt table entry 239

        .word do_nothing_isr           # interrupt table entry 240
        .word do_nothing_isr           # interrupt table entry 241
        .word do_nothing_isr           # interrupt table entry 242
        .word do_nothing_isr           # interrupt table entry 243
        .word do_nothing_isr           # Reserved
        .word do_nothing_isr           # Reserved
        .word do_nothing_isr           # Reserved
        .word do_nothing_isr           # Reserved

        .word do_nothing_isr            # NMI Interrupt
        .word do_nothing_isr           # Reserved
        .word do_nothing_isr           # Reserved
        .word do_nothing_isr           # Reserved
        .word isr3                     # interrupt table entry 252
        .word isr2                     # interrupt table entry 253
        .word isr1                     # interrupt table entry 254
        .word isr0                     # interrupt table entry 255
