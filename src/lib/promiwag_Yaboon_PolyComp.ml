(******************************************************************************)
(*      Copyright (c) 2008, Sebastien Mondet                                  *)
(*                                                                            *)
(*      Permission is hereby granted, free of charge, to any person           *)
(*      obtaining a copy of this software and associated documentation        *)
(*      files (the "Software"), to deal in the Software without               *)
(*      restriction, including without limitation the rights to use,          *)
(*      copy, modify, merge, publish, distribute, sublicense, and/or sell     *)
(*      copies of the Software, and to permit persons to whom the             *)
(*      Software is furnished to do so, subject to the following              *)
(*      conditions:                                                           *)
(*                                                                            *)
(*      The above copyright notice and this permission notice shall be        *)
(*      included in all copies or substantial portions of the Software.       *)
(*                                                                            *)
(*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*      OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT           *)
(*      HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,          *)
(*      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING          *)
(*      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR         *)
(*      OTHER DEALINGS IN THE SOFTWARE.                                       *)
(******************************************************************************)

module Comparisons = struct
    let eqpoly = (=)
    let nepoly = (<>)
    let ltpoly = (<)
    let gtpoly = (>)
    let lepoly = (<=)
    let gepoly = (>=)

    let eqphy = (==)

    let eqi (a:int) (b:int)       = eqpoly a b
    let eqf (a:float) (b:float)   = eqpoly a b
    let nei (a:int) (b:int)       = nepoly a b
    let nef (a:float) (b:float)   = nepoly a b
    let lti (a:int) (b:int)       = ltpoly a b
    let ltf (a:float) (b:float)   = ltpoly a b
    let gti (a:int) (b:int)       = gtpoly a b
    let gtf (a:float) (b:float)   = gtpoly a b
    let lei (a:int) (b:int)       = lepoly a b
    let lef (a:float) (b:float)   = lepoly a b
    let gei (a:int) (b:int)       = gepoly a b
    let gef (a:float) (b:float)   = gepoly a b

    let eqs (a:string) (b:string) = (String.compare a b) = 0
    let nes (a:string) (b:string) = (String.compare a b) <> 0

    let minpoly = min
    let maxpoly = max
    let mini  (a:int) (b:int)      = minpoly a b
    let minf (a:float) (b:float)   = minpoly a b
    let maxi  (a:int) (b:int)      = maxpoly a b
    let maxf (a:float) (b:float)   = maxpoly a b
end
module NoPolyPhy = struct
    type shouldnt_be_used
    let (==) (x:shouldnt_be_used) (y:shouldnt_be_used) = false
    let (!=) (x:shouldnt_be_used) (y:shouldnt_be_used) = false
end

module CompAndNoPolyPhy = struct
    include Comparisons
    include NoPolyPhy
end

module CompAndOveridePoly = struct
    include CompAndNoPolyPhy
    let (=)  = eqi
    let (=.) = eqf
    let (<>)  = nei
    let (<>.) = nef
    let (<)  = lti
    let (<.) = ltf
    let (>)  = gti
    let (>.) = gtf
    let (<=) = lei
    let (<=.)= lef
    let (>=) = gei
    let (>=.)= gef
    let (=$=) = eqs
    let (<$>) = nes
    let (=@=) = eqpoly
    let (<@>) = nepoly
    let min = mini
    let max = maxi
end


