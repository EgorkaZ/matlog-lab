use std::iter;

pub fn preprocess<Iter>(it: Iter) -> String
    where Iter: Iterator<Item = char>
{
    let mut res = String::new();
    let mut dots_stack = vec![0];

    // let's insert fake closing ')' as EOF, so after zero brace level '$' were also inserted
    // this one will be inserted into 'res' string, so will be deleted later
    let mut it = it.chain(")".chars());

    while let Some(curr) = it.next() {
        if curr.is_whitespace() {
            continue;
        }

        match curr {
            '.' => *dots_stack.last_mut().unwrap() += 1,
            '(' => dots_stack.push(0),
            ')' => {
                let dollar_count = dots_stack.pop().unwrap();
                res.extend(iter::repeat('$').take(dollar_count));
            },
            _ => (),
        };
        res.push(curr);
    }
    res.pop();
    res
}
