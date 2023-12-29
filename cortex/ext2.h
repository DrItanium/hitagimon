/*
hitagimon
Copyright (c) 2020-2023, Joshua Scoggins
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
//
// taken from https://wiki.osdev.org/Ext2
// Created by jwscoggins on 12/29/23.
//

#ifndef HITAGIMON_EXT2_H
#define HITAGIMON_EXT2_H
#include <stdint.h>
namespace cortex {
    namespace fs {
        namespace ext2 {
            /**
             * An ext2 superblock
             */
            union __attribute((packed)) Superblock {
                uint8_t bytes[1024];
                struct {
                    uint32_t inodeCount;
                    uint32_t blockCount;
                    uint32_t reservedBlocksForSuperUser;
                    uint32_t unallocatedBlockCount;
                    uint32_t unallocatedInodeCount;
                    uint32_t startingBlockNumber;
                    uint32_t log2BlockSize;
                    uint32_t log2FragmentSize;
                    uint32_t blocksInEachBlockGroup;
                    uint32_t fragmentsInEachBlockGroup;
                    uint32_t lastMountTime;
                    uint32_t lastWrittenTime;
                    uint16_t numMountsSizeLastConsistencyCheck;
                    uint16_t mountsAllowedBeforeAConsistencyCheck;
                    uint16_t signature;
                    uint16_t fileSystemState;
                    uint16_t onErrorDetect;
                    uint16_t minorVersionPortion;
                    uint32_t lastConsistencyCheckTime;
                    uint32_t intervalBetweenForcedConsistencyChecks;
                    uint32_t operatingSystemID;
                    uint32_t majorVersionPortion;
                    uint16_t userIdThatCanReservedBlocks;
                    uint16_t groupIdThatCanUseReservedBlocks;
                    /// @todo implement more fields
                } fields;
            };
        } // end namespace ext2
    } // end namespace fs
} // end namespace cortex
#endif //HITAGIMON_EXT2_H
