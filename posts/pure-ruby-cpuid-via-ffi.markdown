---
published: 2013-07-15 02:37:38
title: Getting CPUID info in pure Ruby
---

Here's a fun little exercise: write a pure Ruby interface to the x86
[cpuid](http://en.wikipedia.org/wiki/CPUID) instruction.

In order to use the `cpuid` instruction from Ruby, we'll need
to craft and execute some machine code. Before we can do that, though,
we'll need to properly allocate the memory for our code.

Executable Memory Allocation
----------------------------

Modern operating systems [mark regions of writable memory as
non-executable](http://en.wikipedia.org/wiki/Executable_space_protection)
to prevent various exploits. Memory is protected per
[page](https://en.wikipedia.org/wiki/Page_(computer_memory)), so we'll
need to allocate the memory for our code such that it is:

-   aligned on a page boundary, and
-   the number of bytes is a multiple of the page size.

Below, you'll see how we can use [FFI](https://github.com/ffi/ffi) to
wrap [posix\_memalign](http://linux.die.net/man/3/posix_memalign) (to
allocate our memory) and [mprotect](http://linux.die.net/man/2/mprotect)
(to set the protection):

```ruby
module MemUtil extend FFI::Library
  ffi_lib "c"
  attach_function :mprotect,       [:pointer, :size_t, :int],    :int
  attach_function :posix_memalign, [:pointer, :size_t, :size_t], :int
  attach_function :getpagesize,    [],                           :int
  attach_function :memset,         [:pointer, :int, :size_t],    :void

  PROT_NONE  = 0x00
  PROT_READ  = 0x01
  PROT_WRITE = 0x02
  PROT_EXEC  = 0x04

  def self.allocate_pages(count)
    out_pointer = FFI::MemoryPointer.new(:pointer, 1)
    posix_memalign(out_pointer, page_size, page_size * count)
    out_pointer.read_pointer
  end

  def self.allocate_executable(code)
    pages = (code.size / page_size.to_f).ceil
    mem   = allocate_pages(pages)
    memset(mem, 0x90, pages * page_size)
    mem.put_array_of_uint8(0, code)
    mprotect(mem, code.size, PROT_READ | PROT_EXEC)

    mem
  end

  def self.page_size
    @page_size ||= getpagesize
  end
end
```

Given an array of bytes, `MemUtil::allocate_executable`
allocates one or more pages of memory, fills it with
[NOPs](http://en.wikipedia.org/wiki/NOP), writes the machine code, and
then sets the protection.

CPUID machine code
------------------

Here's the fun part, where we assemble a
[cdecl](http://en.wikipedia.org/wiki/X86_calling_conventions#cdecl)
function and encode the machine code as an array. I won't go into too
much detail about the assembler [^1], but something like this should do:

```ruby
module CPUID
  X32_CODE = [
    0x55,                     # pushl   %ebp
    0x89, 0xE5,               # movl    %esp,%ebp
    0x57,                     # pushl   %edi
    0x56,                     # pushl   %esi
    0x8B, 0x45, 0x08,         # movl    0x08(%ebp),%eax
    0x89, 0xDE,               # movl    %ebx,%esi
    0x0F, 0xA2,               # cpuid
    0x87, 0xDE,               # xchgl   %esi,%ebx
    0x8B, 0x7D, 0x0C,         # movl    0x0c(%ebp),%edi
    0x89, 0x07,               # movl    %eax,(%edi)
    0x89, 0x77, 0x04,         # movl    %esi,0x04(%edi)
    0x89, 0x4F, 0x08,         # movl    %ecx,0x08(%edi)
    0x89, 0x57, 0x0C,         # movl    %edx,0x0c(%edi)
    0x5E,                     # popl    %esi
    0x5F,                     # popl    %edi
    0x5D,                     # popl    %ebp
    0xC3,                     # ret
  ]

  X64_CODE = [
    0x55,                     # pushq   %rbp
    0x48, 0x89, 0xE5,         # movq    %rsp,%rbp
    0x49, 0x89, 0xF0,         # movq    %rsi,%r8
    0x89, 0xF8,               # movl    %edi,%eax
    0x89, 0xDE,               # movl    %ebx,%esi
    0x0F, 0xA2,               # cpuid
    0x87, 0xDE,               # xchgl   %esi,%ebx
    0x41, 0x89, 0x00,         # movl    %eax,(%r8)
    0x41, 0x89, 0x70, 0x04,   # movl    %esi,0x04(%r8)
    0x41, 0x89, 0x48, 0x08,   # movl    %ecx,0x08(%r8)
    0x41, 0x89, 0x50, 0x0C,   # movl    %edx,0x0c(%r8)
    0x5D,                     # popq    %rbp
    0xC3,                     # ret
  ]
end
```

`X32_CODE` and `X64_CODE` contain our machine
code for 32bit and 64bit architectures, respectively. The functions take
an integer and a pointer to an array of 4 32bit integers; the former is
an argument to `cpuid` (via `eax`) that specifies
what info we want returned, and the later is where we write out the
resulting data.

All that's left now is to set up the trampoline and give it a spin:

```ruby
module CPUID
  CPUID_FUNCTION = \
    FFI::Function.new(:void, [ :uint, :pointer ],
      MemUtil.allocate_executable(FFI::Pointer.size == 4 ? X32_CODE : X64_CODE))

  def self.run_cpuid(fn)
    buffer = FFI::MemoryPointer.new(:uint32, 4)
    CPUID_FUNCTION.call(fn, buffer)
    buffer.get_array_of_uint32(0, 4)
  end
end

vendor_string = CPUID.run_cpuid(0).inject("") do |str, reg|
  0.upto(3) do |idx|
    str << ((reg >> (idx * 8)) & 0xFF).chr
  end
  str
end

puts vendor_string
# On my MacBook, this prints:
# GenuntelineI
```

The Full Listing
----------------

```ruby
require 'ffi'

module CPUID
  X32_CODE = [
    0x55,                     # pushl   %ebp
    0x89, 0xE5,               # movl    %esp,%ebp
    0x57,                     # pushl   %edi
    0x56,                     # pushl   %esi
    0x8B, 0x45, 0x08,         # movl    0x08(%ebp),%eax
    0x89, 0xDE,               # movl    %ebx,%esi
    0x0F, 0xA2,               # cpuid
    0x87, 0xDE,               # xchgl   %esi,%ebx
    0x8B, 0x7D, 0x0C,         # movl    0x0c(%ebp),%edi
    0x89, 0x07,               # movl    %eax,(%edi)
    0x89, 0x77, 0x04,         # movl    %esi,0x04(%edi)
    0x89, 0x4F, 0x08,         # movl    %ecx,0x08(%edi)
    0x89, 0x57, 0x0C,         # movl    %edx,0x0c(%edi)
    0x5E,                     # popl    %esi
    0x5F,                     # popl    %edi
    0x5D,                     # popl    %ebp
    0xC3,                     # ret
  ]

  X64_CODE = [
    0x55,                     # pushq   %rbp
    0x48, 0x89, 0xE5,         # movq    %rsp,%rbp
    0x49, 0x89, 0xF0,         # movq    %rsi,%r8
    0x89, 0xF8,               # movl    %edi,%eax
    0x89, 0xDE,               # movl    %ebx,%esi
    0x0F, 0xA2,               # cpuid
    0x87, 0xDE,               # xchgl   %esi,%ebx
    0x41, 0x89, 0x00,         # movl    %eax,(%r8)
    0x41, 0x89, 0x70, 0x04,   # movl    %esi,0x04(%r8)
    0x41, 0x89, 0x48, 0x08,   # movl    %ecx,0x08(%r8)
    0x41, 0x89, 0x50, 0x0C,   # movl    %edx,0x0c(%r8)
    0x5D,                     # popq    %rbp
    0xC3,                     # ret
  ]

  def self.run_cpuid(fn)
    buffer = FFI::MemoryPointer.new(:uint32, 4)
    CPUID_FUNCTION.call(fn, buffer)
    buffer.get_array_of_uint32(0, 4)
  end

  module MemUtil extend FFI::Library
    ffi_lib "c"
    attach_function :mprotect,       [:pointer, :size_t, :int],    :int
    attach_function :posix_memalign, [:pointer, :size_t, :size_t], :int
    attach_function :getpagesize,    [],                           :int
    attach_function :memset,         [:pointer, :int, :size_t],    :void

    PROT_NONE  = 0x00
    PROT_READ  = 0x01
    PROT_WRITE = 0x02
    PROT_EXEC  = 0x04

    def self.allocate_pages(count)
      out_pointer = FFI::MemoryPointer.new(:pointer, 1)
      posix_memalign(out_pointer, page_size, page_size * count)
      out_pointer.read_pointer
    end

    def self.allocate_executable(code)
      pages = (code.size / page_size.to_f).ceil
      mem   = allocate_pages(pages)
      memset(mem, 0x90, pages * page_size)
      mem.put_array_of_uint8(0, code)
      mprotect(mem, code.size, PROT_READ | PROT_EXEC)

      mem
    end

    def self.page_size
      @page_size ||= getpagesize
    end
  end

  CPUID_FUNCTION = \
    FFI::Function.new(:void, [ :uint, :pointer ],
      MemUtil.allocate_executable(FFI::Pointer.size == 4 ? X32_CODE : X64_CODE))
end
```

[^1]: If you're new to assembly programming, for a first book I highly
    recommend ["Assembly Language Step-by-Step: Programming with
    Linux"](http://www.amazon.com/Assembly-Language-Step—-Step-Programming/dp/0470497025)
    by Jeff Duntemann.
