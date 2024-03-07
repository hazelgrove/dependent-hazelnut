open Terms;
open Lang;

// These functions place a cursor in the first hole, if there is one
let find_hole_name = (x: name): option(zname) =>
  switch (x) {
  | Hole => Some(Cursor(Hole))
  | Text(_) => None
  };

let rec find_hole_typ = (t: typ): option(ztyp) => {
  switch (t) {
  | Hole => Some(Cursor(Hole))
  | Base(_) => None
  | Arrow(t1, t2) =>
    switch (find_hole_typ(t1)) {
    | Some(z) => Some(LArrow(z, t2))
    | None =>
      switch (find_hole_typ(t2)) {
      | Some(z) => Some(RArrow(t1, z))
      | None => None
      }
    }
  };
};

let rec find_hole_exp = (e: exp) =>
  switch (e) {
  | Hole => Some(Cursor(Hole))
  | Mark(m, e) =>
    switch (find_hole_exp(e)) {
    | Some(z) => Some(Mark(m, z))
    | None => None
    }
  | Var(_) => None
  | Fun(x, t, e) =>
    switch (find_hole_name(x)) {
    | Some(z) => Some(XFun(z, t, e))
    | None =>
      switch (find_hole_typ(t)) {
      | Some(z) => Some(TFun(x, z, e))
      | None =>
        switch (find_hole_exp(e)) {
        | Some(z) => Some(EFun(x, t, z))
        | None => None
        }
      }
    }
  | Ap(e1, e2) =>
    switch (find_hole_exp(e1)) {
    | Some(z) => Some(LAp(z, e2))
    | None =>
      switch (find_hole_exp(e2)) {
      | Some(z) => Some(RAp(e1, z))
      | None => None
      }
    }
  | Let(x, t, e1, e2) =>
    switch (find_hole_name(x)) {
    | Some(z) => Some(XLet(z, t, e1, e2))
    | None =>
      switch (find_hole_typ(t)) {
      | Some(z) => Some(TLet(x, z, e1, e2))
      | None =>
        switch (find_hole_exp(e1)) {
        | Some(z) => Some(E1Let(x, t, z, e2))
        | None =>
          switch (find_hole_exp(e2)) {
          | Some(z) => Some(E2Let(x, t, e1, z))
          | None => None
          }
        }
      }
    }
  };

// These functions move cursor to next hole, if there is one
let find_hole_zname = (z: zname): option(zname) =>
  switch (z) {
  | Cursor(_) => None
  };

let rec find_hole_ztyp = (~loop=false, z: ztyp): option(ztyp) => {
  switch (z) {
  | Cursor(_) => None
  | LArrow(z, t) =>
    switch (find_hole_ztyp(z)) {
    | Some(z') => Some(LArrow(z', t))
    | None =>
      switch (find_hole_typ(t)) {
      | Some(z') => Some(RArrow(typ_of_ztyp(z), z'))
      | None =>
        switch (find_hole_ztyp(~loop, z)) {
        | Some(z') => Some(LArrow(z', t))
        | None => None
        }
      }
    }
  | RArrow(t, z) =>
    switch (find_hole_ztyp(z)) {
    | Some(z') => Some(RArrow(t, z'))
    | None =>
      if (loop) {
        switch (find_hole_typ(t)) {
        | Some(z') => Some(LArrow(z', typ_of_ztyp(z)))
        | None =>
          switch (find_hole_ztyp(~loop=true, z)) {
          | Some(z') => Some(RArrow(t, z'))
          | None => None
          }
        };
      } else {
        None;
      }
    }
  };
};

let rec find_hole_zexp = (~loop=false, z: zexp) => {
  switch (z) {
  | Cursor(_) => None
  | Mark(m, z) =>
    switch (find_hole_zexp(z)) {
    | Some(z) => Some(Mark(m, z))
    | None => None
    }
  | XFun(z, t, e) =>
    switch (find_hole_zname(z)) {
    | Some(z') => Some(XFun(z', t, e))
    | None =>
      switch (find_hole_typ(t)) {
      | Some(z') => Some(TFun(name_of_zname(z), z', e))
      | None =>
        switch (find_hole_exp(e)) {
        | Some(z') => Some(EFun(name_of_zname(z), t, z'))
        | None => None
        }
      }
    }
  | TFun(x, z, e) =>
    switch (find_hole_ztyp(z)) {
    | Some(z') => Some(TFun(x, z', e))
    | None =>
      switch (find_hole_exp(e)) {
      | Some(z') => Some(EFun(x, typ_of_ztyp(z), z'))
      | None =>
        if (loop) {
          switch (find_hole_name(x)) {
          | Some(z') => Some(XFun(z', typ_of_ztyp(z), e))
          | None =>
            switch (find_hole_ztyp(~loop=true, z)) {
            | Some(z') => Some(TFun(x, z', e))
            | None => None
            }
          };
        } else {
          None;
        }
      }
    }
  | EFun(x, t, z) =>
    switch (find_hole_zexp(z)) {
    | Some(z') => Some(EFun(x, t, z'))
    | None =>
      if (loop) {
        switch (find_hole_name(x)) {
        | Some(z') => Some(XFun(z', t, exp_of_zexp(z)))
        | None =>
          switch (find_hole_typ(t)) {
          | Some(z') => Some(TFun(x, z', exp_of_zexp(z)))
          | None =>
            switch (find_hole_zexp(~loop=true, z)) {
            | Some(z') => Some(EFun(x, t, z'))
            | None => None
            }
          }
        };
      } else {
        None;
      }
    }
  | LAp(z, e) =>
    switch (find_hole_zexp(z)) {
    | Some(z') => Some(LAp(z', e))
    | None =>
      switch (find_hole_exp(e)) {
      | Some(z') => Some(RAp(exp_of_zexp(z), z'))
      | None =>
        switch (find_hole_zexp(~loop, z)) {
        | Some(z') => Some(LAp(z', e))
        | None => None
        }
      }
    }
  | RAp(e, z) =>
    switch (find_hole_zexp(z)) {
    | Some(z') => Some(RAp(e, z'))
    | None =>
      if (loop) {
        switch (find_hole_exp(e)) {
        | Some(z') => Some(LAp(z', exp_of_zexp(z)))
        | None =>
          switch (find_hole_zexp(~loop=true, z)) {
          | Some(z') => Some(RAp(e, z'))
          | None => None
          }
        };
      } else {
        None;
      }
    }
  | XLet(z, t, e1, e2) =>
    switch (find_hole_zname(z)) {
    | Some(z') => Some(XLet(z', t, e1, e2))
    | None =>
      switch (find_hole_typ(t)) {
      | Some(z') => Some(TLet(name_of_zname(z), z', e1, e2))
      | None =>
        switch (find_hole_exp(e1)) {
        | Some(z') => Some(E1Let(name_of_zname(z), t, z', e2))
        | None =>
          switch (find_hole_exp(e2)) {
          | Some(z') => Some(E2Let(name_of_zname(z), t, e1, z'))
          | None => None
          }
        }
      }
    }
  | TLet(x, z, e1, e2) =>
    switch (find_hole_ztyp(z)) {
    | Some(z') => Some(TLet(x, z', e1, e2))
    | None =>
      switch (find_hole_exp(e1)) {
      | Some(z') => Some(E1Let(x, typ_of_ztyp(z), z', e2))
      | None =>
        switch (find_hole_exp(e2)) {
        | Some(z') => Some(E2Let(x, typ_of_ztyp(z), e1, z'))
        | None =>
          if (loop) {
            switch (find_hole_name(x)) {
            | Some(z') => Some(XLet(z', typ_of_ztyp(z), e1, e2))
            | None =>
              switch (find_hole_ztyp(~loop=true, z)) {
              | Some(z') => Some(TLet(x, z', e1, e2))
              | None => None
              }
            };
          } else {
            None;
          }
        }
      }
    }
  | E1Let(x, t, z, e2) =>
    switch (find_hole_zexp(z)) {
    | Some(z') => Some(E1Let(x, t, z', e2))
    | None =>
      switch (find_hole_exp(e2)) {
      | Some(z') => Some(E2Let(x, t, exp_of_zexp(z), z'))
      | None =>
        if (loop) {
          switch (find_hole_name(x)) {
          | Some(z') => Some(XLet(z', t, exp_of_zexp(z), e2))
          | None =>
            switch (find_hole_typ(t)) {
            | Some(z') => Some(TLet(x, z', exp_of_zexp(z), e2))
            | None =>
              switch (find_hole_zexp(~loop=true, z)) {
              | Some(z') => Some(E1Let(x, t, z', e2))
              | None => None
              }
            }
          };
        } else {
          None;
        }
      }
    }
  | E2Let(x, t, e1, z) =>
    switch (find_hole_zexp(z)) {
    | Some(z') => Some(E2Let(x, t, e1, z'))
    | None =>
      if (loop) {
        switch (find_hole_name(x)) {
        | Some(z') => Some(XLet(z', t, e1, exp_of_zexp(z)))
        | None =>
          switch (find_hole_typ(t)) {
          | Some(z') => Some(TLet(x, z', e1, exp_of_zexp(z)))
          | None =>
            switch (find_hole_exp(e1)) {
            | Some(z') => Some(E1Let(x, t, z', exp_of_zexp(z)))
            | None =>
              switch (find_hole_zexp(~loop=true, z)) {
              | Some(z') => Some(E2Let(x, t, e1, z'))
              | None => None
              }
            }
          }
        };
      } else {
        None;
      }
    }
  };
};
