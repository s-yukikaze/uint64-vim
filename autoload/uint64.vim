" Author:  Sakura-yukikaze <sakura_yukikaze@live.jp>
" Version: 0.0.1
" License: MIT License (see below)
" {{{
" Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
"
" The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}


let s:save_cpo = &cpo
set cpo&vim

" Simple unit test
function! uint64#_unit()"{{{
  let a = copy(s:UInt64).init("0x123456789ABC")
  let b = copy(s:UInt64).init("0x876543210FED")
  let c = a.add(b)
  echo printf("0x%s + 0x%s = 0x%s", a.str(16), b.str(16), c.str(16))
  let d = b.sub(a)
  echo printf("0x%s - 0x%s = 0x%s", b.str(16), a.str(16), d.str(16))
  let e = a.mul(b)
  echo printf("0x%s x 0x%s = 0x%s", a.str(16), b.str(16), e.str(16))
  let [f,g] = b.divmod(a)
  echo printf("0x%s / 0x%s = 0x%s ... 0x%s", b.str(16), a.str(16), f.str(16), g.str(16))
endfunction"}}}

function! uint64#val(...)"{{{
  let val = copy(s:UInt64)
  return call(val.init, a:000, val)
endfunction"}}}

let s:UInt64 = {}

function! s:UInt64.init(...)"{{{
  if a:0 == 0
    self.initWithWords(0, 0)
  elseif a:0 == 1
    if type(a:1) == type(0)
      return self.initWithWords(0, a:1)
    elseif type(a:1) == type("")
      return self.initWithString(a:1)
    else
      echoerr "Invalid argument type."
    endif
  elseif a:0 == 2
    return self.initWithWords(a:1, a:2)
  else
    echoerr "Invalid number of argument."
  endif
endfunction"}}}

function! s:UInt64.initWithWords(high, low)"{{{
  let self._high = a:high
  let self._low = a:low
  return self
endfunction"}}}

function! s:UInt64.initWithString(val)"{{{
  let ret = copy(s:UInt64).initWithWords(0, 0)
  if a:val =~ '^0x\x\+$'
    " Hexadecimal
    for e in split(a:val[2:], '\zs')
      let ret = ret.shl(4).add(str2nr(e, 16))
    endfor
  elseif a:val =~ '^\d\+$'
    " Decimal
    for e in split(a:val, '\zs')
      let ret = ret.mul(10).add(str2nr(e, 10))
    endfor
  else
    echoerr "Invalid argument"
  endif
  return self.initWithWords(ret._high, ret._low)
endfunction"}}}

function! s:UInt64_cast(val)"{{{
  if type(a:val) == type(0) || type(a:val) == type("") 
    return copy(s:UInt64).init(a:val)
  elseif type(a:val) == type({})
    return a:val
  else
    echoerr "Could not cast an value to UInt64"
  endif
endfunction"}}} 

function! s:UInt64.add_d(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  let [self._low, carry] = s:uint32_add(self._low, rval._low)
  let [self._high, carry] = s:uint32_add(self._high, rval._high, carry)
  return self 
endfunction"}}}

function! s:UInt64.add(rval)"{{{
  return copy(self).add_d(a:rval)
endfunction"}}}

function! s:UInt64.sub_d(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  let [self._low, borrow] = s:uint32_sub(self._low, rval._low)
  let [self._high, borrow] = s:uint32_sub(self._high, rval._high, borrow)
  return self
endfunction"}}}

function! s:UInt64.sub(rval)"{{{
  return copy(self).sub_d(a:rval)
endfunction"}}}

function! s:UInt64.mul_d(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  let l = self._as_words()
  let r = rval._as_words()
  let [a0, carry] = s:uint16_add(l[0] * r[0], 0)
  let [a1, carry] = s:uint16_add(l[1] * r[0], l[0] * r[1], carry)
  let [a2, carry] = s:uint16_add(l[2] * r[0], l[1] * r[1], l[0] * r[2], carry)
  let [a3, carry] = s:uint16_add(l[3] * r[0], l[2] * r[1], l[1] * r[2], l[0] * r[3], carry)
  let self._low = a0 + a1 * 0x10000
  let self._high = a2 + a3 * 0x10000
  return self
endfunction"}}}

function! s:UInt64.mul(rval)"{{{
  return copy(self).mul_d(a:rval)
endfunction"}}}

function! s:UInt64.div(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  return self.divmod(rval)[0]
endfunction"}}}

function! s:UInt64.mod(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  return self.divmod(rval)[1]
endfunction"}}}

function! s:UInt64.divmod(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  if rval._high == 0 && rval._low == 0
    throw "UInt64: divided by zero"
  endif
  if self.lt(a:rval)
    return [copy(s:UInt64).init(0), copy(self)]
  endif
  let divisor = rval
  let reminder = self
  let shift = 0
  let divisor_cache = [divisor]
  " Normalize the divisor.
  while divisor._high >= 0 
    let divisor = divisor.shl(1)
    call add(divisor_cache, divisor)
    let shift += 1
  endwhile
  " Do division.
  let quotient = copy(s:UInt64).init(0)
  while shift
    let quotient = quotient.shl(1)
    if divisor_cache[shift - 1].lt_eq(reminder)
      call quotient.add_d(1)
      call reminder.sub_d(divisor_cache[shift - 1])
    endif
    let shift -= 1
  endwhile
  return [quotient, reminder]
endfunction"}}}

function! s:UInt64.and(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  let high = s:uint32_and(self._high, rval._high)
  let low = s:uint32_and(self._low, rval._low)
  return copy(s:UInt64).init(high, low)
endfunction"}}}

function! s:UInt64.or(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  let high = or(self._high, rval._high)
  let low = or(self._low, rval._low)
  return copy(s:UInt64).init(high, low)
endfunction"}}}

function! s:UInt64.shl(rval)"{{{
  if a:rval < 0
    echoerr "negative shl not permitted."
  elseif a:rval < 32
    let low = s:uint32_shl(self._low, a:rval)
    let high = s:uint32_shl(self._high, a:rval)
    let carry = s:uint32_shr(self._low, 32 - a:rval)
    return copy(s:UInt64).init(high + carry, low)
  else
    let rval = a:rval - 32
    return copy(s:UInt64).init(s:uint32_shl(self._low, rval), 0)
  endif
endfunction"}}} 
  
function! s:UInt64.shr(rval)"{{{
  if a:rval < 0
    echoerr "negative shr not permitted."
  elseif a:rval < 32
    let low = s:uint32_shr(self._low, a:rval)
    let high = s:uint32_shr(self._high, a:rval)
    let lcarry = s:uint32_shl(self._high, 32 - a:rval)
    return copy(s:UInt64).init(high, low + lcarry)
  else
    let rval = a:rval - 32
    return copy(s:UInt64).init(0, s:uint32_shr(self._high, rval), 0)
  endif
endfunction"}}} 

function! s:UInt64.lt(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  return s:uint32_lt(self._high, rval._high) ? 1 :
    \    self._high == rval._high &&
    \    s:uint32_lt(self._low, rval._low)   ? 1 : 0
endfunction"}}}

function! s:UInt64.lt_eq(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  return s:uint32_lt(self._high, rval._high)  ? 1 :
    \    self._high == rval._high &&
    \    s:uint32_lt_eq(self._low, rval._low) ? 1 : 0
endfunction"}}}

function! s:UInt64.gt(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  return s:uint32_lt(rval._high, self._high) ? 1 :
    \    self._high == rval._high &&
    \    s:uint32_lt(rval._low, self._low)   ? 1 : 0
endfunction"}}}

function! s:UInt64.gt_eq(rval)"{{{
  let rval = s:UInt64_cast(a:rval)
  return s:uint32_lt(rval._high, self._high)  ? 1 :
    \    self._high == rval._high &&
    \    s:uint32_lt_eq(rval._low, self._low) ? 1 : 0
endfunction"}}}

function! s:UInt64.eq(rval)"{{{
  return self._high == rval._high &&
         self._low == rval._low
endfunction"}}}

function! s:UInt64.is_zero()"{{{
  return self._high == 0 && self._low == 0
endfunction"}}}

function! s:UInt64.str(base)"{{{
  if a:base == 16
    if self._high != 0
      return printf('%X%08X', self._high, self._low)
    else
      return printf('%X', self._low)
    endif
  if a:base >= 0 && a:base <= 16
    let dig2chr = "0123456789ABCDEF"
    let str = ""
    let tmp = copy(s:UInt64).init(self._high, self._low)
    while tmp._high != 0 || tmp._low != 0 
      let [tmp, r] = tmp.divmod(a:base)
      let str = dig2chr[r._low] . str 
    endwhile
    return str
  else
    echoerr "Invalid base."
  endif
endfunction"}}}

function! s:UInt64._as_words()"{{{
  return [
    \ s:uint32_and(self._low, 0xFFFF), s:uint32_shr(self._low, 16),
    \ s:uint32_and(self._high, 0xFFFF), s:uint32_shr(self._high, 16)
    \]
endfunction"}}}

function! s:uint32_remove_msb(val)"{{{
  return a:val < 0 ? [a:val + 0x80000000, 1] : [a:val, 0]
endfunction"}}}

function! s:uint32_lt(lval, rval)"{{{
  let [lval, lmsb] = s:uint32_remove_msb(a:lval)
  let [rval, rmsb] = s:uint32_remove_msb(a:rval)
  return lmsb < rmsb ? 1 : lmsb == rmsb && lval < rval ? 1 : 0
endfunction"}}}

function! s:uint32_lt_eq(lval, rval)"{{{
  let [lval, lmsb] = s:uint32_remove_msb(a:lval)
  let [rval, rmsb] = s:uint32_remove_msb(a:rval)
  return a:lval == a:rval ? 1 : lmsb < rmsb ? 1 : lmsb == rmsb && lval < rval ? 1 : 0
endfunction"}}}

function! s:uint32_shl(val, shift)"{{{
  return a:val * s:uint32_pow2[a:shift]
endfunction"}}}

function! s:uint32_shr(val, shift)"{{{
  let [val, msb] = s:uint32_remove_msb(a:val) 
  return val / s:uint32_pow2[a:shift] + msb * s:uint32_pow2[31 - a:shift]
endfunction"}}}

let s:uint32_pow2 = [
  \ 0x1, 0x2, 0x4, 0x8,
  \ 0x10, 0x20, 0x40, 0x80,
  \ 0x100, 0x200, 0x400, 0x800,
  \ 0x1000, 0x2000, 0x4000, 0x8000,
  \ 0x10000, 0x20000, 0x40000, 0x80000,
  \ 0x100000, 0x20000, 0x400000, 0x800000,
  \ 0x1000000, 0x2000000, 0x4000000, 0x8000000,
  \ 0x10000000, 0x20000000, 0x40000000, 0x80000000
  \]

function! s:uint32_and(lval, rval)"{{{
  let [lval, lmsb] = s:uint32_remove_msb(a:lval)
  let [rval, rmsb] = s:uint32_remove_msb(a:rval)
  let aval = (lmsb == 1 && rmsb == 1) ? 0x80000000 : 0
  for i in range(8) 
    let aval += s:uint8_and[lval % 0x10][rval % 0x10] * s:uint32_pow2[i * 4]
    let lval = lval / 0x10
    let rval = rval / 0x10
  endfor
  return aval
endfunction"}}}

function! s:uint32_or(lval, rval)"{{{
  let [lval, lmsb] = s:uint32_remove_msb(a:lval)
  let [rval, rmsb] = s:uint32_remove_msb(a:rval)
  let aval = (lmsb == 1 || rmsb == 1) ? 0x80000000 : 0
  for i in range(8) 
    let aval += s:uint8_or[lval % 0x10][rval % 0x10] * s:uint32_pow2[i * 4]
    let lval = lval / 0x10
    let rval = rval / 0x10
  endfor
  return aval
endfunction"}}}

let s:uint8_and = [
  \ [0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00],
  \ [0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01],
  \ [0x00 ,0x00 ,0x02 ,0x02 ,0x00 ,0x00 ,0x02 ,0x02 ,0x00 ,0x00 ,0x02 ,0x02 ,0x00 ,0x00 ,0x02 ,0x02],
  \ [0x00 ,0x01 ,0x02 ,0x03 ,0x00 ,0x01 ,0x02 ,0x03 ,0x00 ,0x01 ,0x02 ,0x03 ,0x00 ,0x01 ,0x02 ,0x03],
  \ [0x00 ,0x00 ,0x00 ,0x00 ,0x04 ,0x04 ,0x04 ,0x04 ,0x00 ,0x00 ,0x00 ,0x00 ,0x04 ,0x04 ,0x04 ,0x04],
  \ [0x00 ,0x01 ,0x00 ,0x01 ,0x04 ,0x05 ,0x04 ,0x05 ,0x00 ,0x01 ,0x00 ,0x01 ,0x04 ,0x05 ,0x04 ,0x05],
  \ [0x00 ,0x00 ,0x02 ,0x02 ,0x04 ,0x04 ,0x06 ,0x06 ,0x00 ,0x00 ,0x02 ,0x02 ,0x04 ,0x04 ,0x06 ,0x06],
  \ [0x00 ,0x01 ,0x02 ,0x03 ,0x04 ,0x05 ,0x06 ,0x07 ,0x00 ,0x01 ,0x02 ,0x03 ,0x04 ,0x05 ,0x06 ,0x07],
  \ [0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x00 ,0x08 ,0x08 ,0x08 ,0x08 ,0x08 ,0x08 ,0x08 ,0x08],
  \ [0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x00 ,0x01 ,0x08 ,0x09 ,0x08 ,0x09 ,0x08 ,0x09 ,0x08 ,0x09],
  \ [0x00 ,0x00 ,0x02 ,0x02 ,0x00 ,0x00 ,0x02 ,0x02 ,0x08 ,0x08 ,0x0a ,0x0a ,0x08 ,0x08 ,0x0a ,0x0a],
  \ [0x00 ,0x01 ,0x02 ,0x03 ,0x00 ,0x01 ,0x02 ,0x03 ,0x08 ,0x09 ,0x0a ,0x0b ,0x08 ,0x09 ,0x0a ,0x0b],
  \ [0x00 ,0x00 ,0x00 ,0x00 ,0x04 ,0x04 ,0x04 ,0x04 ,0x08 ,0x08 ,0x08 ,0x08 ,0x0c ,0x0c ,0x0c ,0x0c],
  \ [0x00 ,0x01 ,0x00 ,0x01 ,0x04 ,0x05 ,0x04 ,0x05 ,0x08 ,0x09 ,0x08 ,0x09 ,0x0c ,0x0d ,0x0c ,0x0d],
  \ [0x00 ,0x00 ,0x02 ,0x02 ,0x04 ,0x04 ,0x06 ,0x06 ,0x08 ,0x08 ,0x0a ,0x0a ,0x0c ,0x0c ,0x0e ,0x0e],
  \ [0x00 ,0x01 ,0x02 ,0x03 ,0x04 ,0x05 ,0x06 ,0x07 ,0x08 ,0x09 ,0x0a ,0x0b ,0x0c ,0x0d ,0x0e ,0x0f]
  \]

let s:uint8_or = [
  \ [0x00 ,0x01 ,0x02 ,0x03 ,0x04 ,0x05 ,0x06 ,0x07 ,0x08 ,0x09 ,0x0a ,0x0b ,0x0c ,0x0d ,0x0e ,0x0f],
  \ [0x01 ,0x01 ,0x03 ,0x03 ,0x05 ,0x05 ,0x07 ,0x07 ,0x09 ,0x09 ,0x0b ,0x0b ,0x0d ,0x0d ,0x0f ,0x0f],
  \ [0x02 ,0x03 ,0x02 ,0x03 ,0x06 ,0x07 ,0x06 ,0x07 ,0x0a ,0x0b ,0x0a ,0x0b ,0x0e ,0x0f ,0x0e ,0x0f],
  \ [0x03 ,0x03 ,0x03 ,0x03 ,0x07 ,0x07 ,0x07 ,0x07 ,0x0b ,0x0b ,0x0b ,0x0b ,0x0f ,0x0f ,0x0f ,0x0f],
  \ [0x04 ,0x05 ,0x06 ,0x07 ,0x04 ,0x05 ,0x06 ,0x07 ,0x0c ,0x0d ,0x0e ,0x0f ,0x0c ,0x0d ,0x0e ,0x0f],
  \ [0x05 ,0x05 ,0x07 ,0x07 ,0x05 ,0x05 ,0x07 ,0x07 ,0x0d ,0x0d ,0x0f ,0x0f ,0x0d ,0x0d ,0x0f ,0x0f],
  \ [0x06 ,0x07 ,0x06 ,0x07 ,0x06 ,0x07 ,0x06 ,0x07 ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f],
  \ [0x07 ,0x07 ,0x07 ,0x07 ,0x07 ,0x07 ,0x07 ,0x07 ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f],
  \ [0x08 ,0x09 ,0x0a ,0x0b ,0x0c ,0x0d ,0x0e ,0x0f ,0x08 ,0x09 ,0x0a ,0x0b ,0x0c ,0x0d ,0x0e ,0x0f],
  \ [0x09 ,0x09 ,0x0b ,0x0b ,0x0d ,0x0d ,0x0f ,0x0f ,0x09 ,0x09 ,0x0b ,0x0b ,0x0d ,0x0d ,0x0f ,0x0f],
  \ [0x0a ,0x0b ,0x0a ,0x0b ,0x0e ,0x0f ,0x0e ,0x0f ,0x0a ,0x0b ,0x0a ,0x0b ,0x0e ,0x0f ,0x0e ,0x0f],
  \ [0x0b ,0x0b ,0x0b ,0x0b ,0x0f ,0x0f ,0x0f ,0x0f ,0x0b ,0x0b ,0x0b ,0x0b ,0x0f ,0x0f ,0x0f ,0x0f],
  \ [0x0c ,0x0d ,0x0e ,0x0f ,0x0c ,0x0d ,0x0e ,0x0f ,0x0c ,0x0d ,0x0e ,0x0f ,0x0c ,0x0d ,0x0e ,0x0f],
  \ [0x0d ,0x0d ,0x0f ,0x0f ,0x0d ,0x0d ,0x0f ,0x0f ,0x0d ,0x0d ,0x0f ,0x0f ,0x0d ,0x0d ,0x0f ,0x0f],
  \ [0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f ,0x0e ,0x0f],
  \ [0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f ,0x0f],
  \]

function! s:uint16_add(...)"{{{
  if a:0 == 2
    let ans = a:1 + a:2
    return [s:uint32_and(ans, 0xFFFF), s:uint32_shr(ans, 16)]
  else
    let [ans, carry1] = call("s:uint16_add", a:000[1:])
    let [ans, carry2] = s:uint16_add(a:1, ans)
    return [ans, carry1 + carry2]
  endif
endfunction"}}}

function! s:uint32_add(...)"{{{
  if a:0 == 2
    let [a1, msb1] = s:uint32_remove_msb(a:1)
    let [a2, msb2] = s:uint32_remove_msb(a:2)
    let [ans, msb3] = s:uint32_remove_msb(a1 + a2)
    let msb = msb1 + msb2 + msb3
    let carry = s:uint32_shr(msb, 1)
    let ans = ans + s:uint32_shl(s:uint32_and(msb, 1), 31) 
    return [ans, carry]
  else
    let [ans, carry1] = call("s:uint32_add", a:000[1:])
    let [ans, carry2] = s:uint32_add(a:1, ans)
    return [ans, carry1 + carry2]
  endif
endfunction"}}}

function! s:uint32_sub(...)"{{{
  if a:0 == 2
    let borrow = s:uint32_lt(a:1, a:2) ? 1 : 0
    return [a:1 - a:2, borrow]
  else
    let [ans, carry] = call("s:uint32_add", a:000[1:])
    let [ans, borrow] = s:uint32_sub(a:1, ans)
    return [ans, carry + borrow]
  endif
endfunction"}}}

" vim: foldmethod=marker
