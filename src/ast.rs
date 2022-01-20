
type Symbol = String;

#[derive(PartialEq, Eq)]
pub enum Statement {
    Public { wire: Symbol },
    Alias {
        name: Symbol,
        inputs: Vec<Symbol>,
        outputs: Vec<Symbol>,
        constraints : Vec<Constraint>
    },
    Constraint {},
    Zero,
}

#[derive(PartialEq, Eq)]
pub enum Constraint {
    Equal,
    Gate
}

#[derive(PartialEq, Eq)]
pub enum Expression {
    Gate,
    Poly { op : Symbol}
}

#[derive(PartialEq, Eq)]
pub enum Wire {
}
