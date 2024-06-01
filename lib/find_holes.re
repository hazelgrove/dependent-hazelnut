open Terms;
open Lang;

// These functions place a cursor in the first hole, if there is one
let find_hole_name =
    (
      _:
        //x
        name,
    )
    : option(zname) =>
  None;
// switch (x) {
// | Hole => Some(Cursor(Hole))
// | Text(_) => None
// };

let rec find_hole_term = (e: pterm) =>
  switch (e) {
  | Hole => Some(Cursor(Hole))
  | Typ
  | Var(_) => None
  | Arrow(x, t1, t2) =>
    switch (find_hole_name(x)) {
    | Some(z) => Some(XArrow(z, t1, t2))
    | None =>
      switch (find_hole_term(t1)) {
      | Some(z) => Some(LArrow(x, z, t2))
      | None =>
        switch (find_hole_term(t2)) {
        | Some(z) => Some(RArrow(x, t1, z))
        | None => None
        }
      }
    }
  | Fun(x, t, e) =>
    switch (find_hole_name(x)) {
    | Some(z) => Some(XFun(z, t, e))
    | None =>
      switch (find_hole_term(t)) {
      | Some(z) => Some(TFun(x, z, e))
      | None =>
        switch (find_hole_term(e)) {
        | Some(z) => Some(EFun(x, t, z))
        | None => None
        }
      }
    }
  | Ap(e1, e2) =>
    switch (find_hole_term(e1)) {
    | Some(z) => Some(LAp(z, e2))
    | None =>
      switch (find_hole_term(e2)) {
      | Some(z) => Some(RAp(e1, z))
      | None => None
      }
    }
  | Let(x, t, e1, e2) =>
    switch (find_hole_name(x)) {
    | Some(z) => Some(XLet(z, t, e1, e2))
    | None =>
      switch (find_hole_term(t)) {
      | Some(z) => Some(TLet(x, z, e1, e2))
      | None =>
        switch (find_hole_term(e1)) {
        | Some(z) => Some(E1Let(x, t, z, e2))
        | None =>
          switch (find_hole_term(e2)) {
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

let rec find_hole_zterm = (~loop=false, z: zterm) => {
  switch (z) {
  | Cursor(_) => None
  | XArrow(z, t1, t2) =>
    switch (find_hole_zname(z)) {
    | Some(z') => Some(XArrow(z', t1, t2))
    | None =>
      switch (find_hole_term(t1)) {
      | Some(z') => Some(LArrow(name_of_zname(z), z', t2))
      | None =>
        switch (find_hole_term(t2)) {
        | Some(z') => Some(RArrow(name_of_zname(z), t1, z'))
        | None => None
        }
      }
    }
  | LArrow(x, z, t2) =>
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(LArrow(x, z', t2))
    | None =>
      switch (find_hole_term(t2)) {
      | Some(z') => Some(RArrow(x, pterm_of_zterm(z), z'))
      | None =>
        if (loop) {
          switch (find_hole_name(x)) {
          | Some(z') => Some(XArrow(z', pterm_of_zterm(z), t2))
          | None =>
            switch (find_hole_zterm(~loop=true, z)) {
            | Some(z') => Some(LArrow(x, z', t2))
            | None => None
            }
          };
        } else {
          None;
        }
      }
    }
  | RArrow(x, t1, z) =>
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(RArrow(x, t1, z'))
    | None =>
      if (loop) {
        switch (find_hole_name(x)) {
        | Some(z') => Some(XArrow(z', t1, pterm_of_zterm(z)))
        | None =>
          switch (find_hole_term(t1)) {
          | Some(z') => Some(LArrow(x, z', pterm_of_zterm(z)))
          | None =>
            switch (find_hole_zterm(~loop=true, z)) {
            | Some(z') => Some(RArrow(x, t1, z'))
            | None => None
            }
          }
        };
      } else {
        None;
      }
    }
  | XFun(z, t, e) =>
    switch (find_hole_zname(z)) {
    | Some(z') => Some(XFun(z', t, e))
    | None =>
      switch (find_hole_term(t)) {
      | Some(z') => Some(TFun(name_of_zname(z), z', e))
      | None =>
        switch (find_hole_term(e)) {
        | Some(z') => Some(EFun(name_of_zname(z), t, z'))
        | None => None
        }
      }
    }
  | TFun(x, z, e) =>
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(TFun(x, z', e))
    | None =>
      switch (find_hole_term(e)) {
      | Some(z') => Some(EFun(x, pterm_of_zterm(z), z'))
      | None =>
        if (loop) {
          switch (find_hole_name(x)) {
          | Some(z') => Some(XFun(z', pterm_of_zterm(z), e))
          | None =>
            switch (find_hole_zterm(~loop=true, z)) {
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
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(EFun(x, t, z'))
    | None =>
      if (loop) {
        switch (find_hole_name(x)) {
        | Some(z') => Some(XFun(z', t, pterm_of_zterm(z)))
        | None =>
          switch (find_hole_term(t)) {
          | Some(z') => Some(TFun(x, z', pterm_of_zterm(z)))
          | None =>
            switch (find_hole_zterm(~loop=true, z)) {
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
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(LAp(z', e))
    | None =>
      switch (find_hole_term(e)) {
      | Some(z') => Some(RAp(pterm_of_zterm(z), z'))
      | None =>
        switch (find_hole_zterm(~loop, z)) {
        | Some(z') => Some(LAp(z', e))
        | None => None
        }
      }
    }
  | RAp(e, z) =>
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(RAp(e, z'))
    | None =>
      if (loop) {
        switch (find_hole_term(e)) {
        | Some(z') => Some(LAp(z', pterm_of_zterm(z)))
        | None =>
          switch (find_hole_zterm(~loop=true, z)) {
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
      switch (find_hole_term(t)) {
      | Some(z') => Some(TLet(name_of_zname(z), z', e1, e2))
      | None =>
        switch (find_hole_term(e1)) {
        | Some(z') => Some(E1Let(name_of_zname(z), t, z', e2))
        | None =>
          switch (find_hole_term(e2)) {
          | Some(z') => Some(E2Let(name_of_zname(z), t, e1, z'))
          | None => None
          }
        }
      }
    }
  | TLet(x, z, e1, e2) =>
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(TLet(x, z', e1, e2))
    | None =>
      switch (find_hole_term(e1)) {
      | Some(z') => Some(E1Let(x, pterm_of_zterm(z), z', e2))
      | None =>
        switch (find_hole_term(e2)) {
        | Some(z') => Some(E2Let(x, pterm_of_zterm(z), e1, z'))
        | None =>
          if (loop) {
            switch (find_hole_name(x)) {
            | Some(z') => Some(XLet(z', pterm_of_zterm(z), e1, e2))
            | None =>
              switch (find_hole_zterm(~loop=true, z)) {
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
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(E1Let(x, t, z', e2))
    | None =>
      switch (find_hole_term(e2)) {
      | Some(z') => Some(E2Let(x, t, pterm_of_zterm(z), z'))
      | None =>
        if (loop) {
          switch (find_hole_name(x)) {
          | Some(z') => Some(XLet(z', t, pterm_of_zterm(z), e2))
          | None =>
            switch (find_hole_term(t)) {
            | Some(z') => Some(TLet(x, z', pterm_of_zterm(z), e2))
            | None =>
              switch (find_hole_zterm(~loop=true, z)) {
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
    switch (find_hole_zterm(z)) {
    | Some(z') => Some(E2Let(x, t, e1, z'))
    | None =>
      if (loop) {
        switch (find_hole_name(x)) {
        | Some(z') => Some(XLet(z', t, e1, pterm_of_zterm(z)))
        | None =>
          switch (find_hole_term(t)) {
          | Some(z') => Some(TLet(x, z', e1, pterm_of_zterm(z)))
          | None =>
            switch (find_hole_term(e1)) {
            | Some(z') => Some(E1Let(x, t, z', pterm_of_zterm(z)))
            | None =>
              switch (find_hole_zterm(~loop=true, z)) {
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
