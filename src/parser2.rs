// use std::{fmt, ops::BitOr, process::Output};

// // trait FullParser<S> {
// //     type Input;
// //     type Output;
// //     type Error;

// //     fn parse(self, stream: S) -> Result<Self::Output, Self::Error>;
// // }

// // trait ParserError: fmt::Display {}

// pub(crate) trait Parser<S>
// where
//     S: Iterator<Item = (usize, Self::Input)>,
// {
//     type Input;
//     type Output;

//     fn parse(self, stream: S) -> Option<(Self::Output, S)>;
// }

// pub(crate) trait Parsable<I, S, P>
// where
//     S: Iterator<Item = (usize, I)>,
//     P: Parser<S, Input = I, Output = Self>,
// {
//     fn make_parser() -> P;
// }

// pub(crate) trait ParserExt<S>: Parser<S> + Sized
// where
//     S: Iterator<Item = (usize, Self::Input)>,
// {
//     fn eq<I, O>(target: &I, output: O) -> ParseEq<I, O> {
//         ParseEq(target, output)
//     }

//     fn filter_map<O, F>(filter_map: F) -> FilterMap<F>
//     where
//         F: FnOnce(&Self::Input) -> Option<O>,
//     {
//         FilterMap(filter_map)
//     }

//     fn any(parsers: Vec<Self>) -> Any<Self> {
//         Any(parsers)
//     }

//     //     fn map<F, O>(self, f: F) -> Map<Self, F>
//     //     where
//     //         F: Fn(Self::Output) -> O,
//     //     {
//     //         Map(self, f)
//     //     }

//     fn or<P2>(self, other: P2) -> Or<Self, P2>
//     where
//         P2: Parser<S, Input = Self::Input, Output = Self::Output>,
//     {
//         Or(self, other)
//     }

//     //     fn and<P2>(self, other: P2) -> And<Self, P2>
//     //     where
//     //         P2: Parser<S, Input = Self::Input, Error = Self::Error>,
//     //     {
//     //         And(self, other)
//     //     }

//     //     fn group<'a>(self, left: &'a Self::Input, right: &'a Self::Input) -> Group<'a, Self::Input, Self> where Self::Input: PartialEq{
//     //         Group(left, self, right)
//     //     }

//     //     fn end(self) -> End<Self> {
//     //         End(self)
//     //     }
// }

// impl<P: Parser<S>, S> ParserExt<S> for P where S: Iterator<Item = (usize, Self::Input)> {}

// pub(crate) struct ParseEq<'a, I, O>(&'a I, O);

// impl<'a, I, O, S> Parser<S> for ParseEq<'a, I, O>
// where
//     I: PartialEq,
//     S: Iterator<Item = (usize, I)>,
// {
//     type Input = I;
//     type Output = O;

//     fn parse(self, mut stream: S) -> Option<(Self::Output, S)> {
//         if let Some((i, el)) = stream.next()
//             && &el == self.0
//         {
//             Some((self.1, stream))
//         } else {
//             None
//         }
//     }
// }

// pub(crate) struct FilterMap<F>(F);

// impl<I, O, S, F> Parser<S> for FilterMap<F>
// where
//     F: FnOnce(I) -> Option<O>,
//     S: Iterator<Item = (usize, I)>,
// {
//     type Input = I;
//     type Output = O;

//     fn parse(self, mut stream: S) -> Option<(Self::Output, S)> {
//         if let Some((i, el)) = stream.next() {
//             if let Some(out) = self.0(el) {
//                 Some((out, stream))
//             } else {
//                 None
//             }
//         } else {
//             None
//         }
//     }
// }

// pub(crate) struct Any<P>(Vec<P>);

// impl<'a, I, O, S, P> Parser<S> for Any<P>
// where
//     S: Iterator<Item = (usize, I)> + Clone,
//     P: Parser<S, Input = I, Output = O>,
// {
//     type Input = I;
//     type Output = O;

//     fn parse(self, stream: S) -> Option<(Self::Output, S)> {
//         for parser in self.0 {
//             if let out @ Some(_) = parser.parse(stream.clone()) {
//                 return out;
//             }
//         }
//         None
//     }
// }

// // struct Map<P, F>(P, F);

// // impl<S, O, P, F> Parser<S> for Map<P, F>
// // where
// //     P: Parser<S>,
// //     S: Iterator<Item = (usize, P::Input)>,
// //     F: Fn(P::Output) -> O,
// // {
// //     type Input = P::Input;
// //     type Output = O;
// //     type Error = P::Error;

// //     fn parse(self, stream: S) -> Result<(Self::Output, S), Self::Error> {
// //         let (out, stream) = self.0.parse(stream)?;
// //         Ok((self.1(out), stream))
// //     }
// // }

// struct Or<P1, P2>(P1, P2);

// impl<S, P1, P2> Parser<S> for Or<P1, P2>
// where
//     P1: Parser<S>,
//     P2: Parser<S, Input = P1::Input, Output = P1::Output>,
//     S: Iterator<Item = (usize, P1::Input)> + Clone,
// {
//     type Input = P1::Input;
//     type Output = P1::Output;

//     fn parse(self, stream: S) -> Option<(Self::Output, S)> {
//         self.0.parse(stream.clone()).or(self.1.parse(stream))
//     }
// }

// // struct And<P1, P2>(P1, P2);

// // struct FilterEq<'a, I>(&'a I);

// // impl<'a, I, S> Parser<S> for FilterEq<'a, I>
// // where
// //     I: PartialEq,
// //     S: Iterator<Item = (usize, I)>,
// // {
// //     type Input = I;
// //     type Output = (usize, I);
// //     type Error = FilterEqErr<I>;

// //     fn parse(self, mut stream: S) -> Result<(Self::Output, S), Self::Error> {
// //         if let Some((i, val)) = stream.next() {
// //             if self.0 == &val {
// //                 Ok(((i, val), stream))
// //             } else {
// //                 Err(FilterEqErr::NotEq(i, val))
// //             }
// //         } else {
// //             Err(FilterEqErr::EOI)
// //         }
// //     }
// // }

// // enum FilterEqErr<I> {
// //     NotEq(usize, I),
// //     EOI,
// // }

// // impl<I> Display for FilterEqErr<I> {
// //     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
// //         todo!()
// //     }
// // }

// // impl<I> ParserError for FilterEqErr<I> {

// // }

// // struct Group<'a, I, P>(&'a I, P, &'a I);

// // impl<'a, S, P> Parser<S> for Group<'a, P::Input, P>
// // where
// //     P: Parser<S>,
// //     P::Input: PartialEq,
// //     S: Iterator<Item = (usize, P::Input)>,
// // {
// //     type Input = P::Input;
// //     type Output = P::Output;
// //     type Error = P::Error;

// //     fn parse(self, stream: S) -> ParserResult<S, Self> {
// //         match FilterEq(self.0).parse(stream) {
// //             Ok(((i, val), stream)) => ,
// //             Err((Some((i, val)), ())) => todo!(),
// //         }
// //     }
// // }

// // enum GroupErr {
// //     NotOpened,
// //     ContentErr,
// //     NotClosed,
// // }

// // struct End<P>(P);
// // enum EndError<I, O, E> {
// //     Error(usize, I, E),
// //     Quit(E),
// //     NotEnd(usize, I, O),
// // }

// // impl<P, S> FullParser<S> for End<P>
// // where
// //     P: Parser<S>,
// //     S: Iterator<Item = (usize, P::Input)>,
// // {
// //     type Input = (usize, P::Input);
// //     type Output = P::Output;
// //     type Error = EndError<P::Input, P::Output, P::Error>;

// //     fn parse(self, stream: S) -> Result<Self::Output, Self::Error> {
// //         match self.0.parse(stream) {
// //             Ok((out, mut stream)) => {
// //                 if let Some((i, val)) = stream.next() {
// //                     Err(EndError::NotEnd(i, val, out))
// //                 } else {
// //                     Ok(out)
// //                 }
// //             }
// //             Err((Some((i, val)), err)) => Err(EndError::Error(i, val, err)),
// //             Err((None, err)) => Err(EndError::Quit(err)),
// //         }
// //     }
// // }

// // // trait BaseParserMut<S>
// // // where
// // //     S: Iterator<Item = Self::Input>,
// // // {
// // //     type Input;
// // //     type Output;
// // //     type Error;

// // //     fn parse(&mut self, stream: S) -> Result<(Self::Output, S), Self::Error>;
// // // }

// // // impl<P, S> BaseParserOnce<S> for P
// // // where
// // //     P: BaseParserMut<S>,
// // //     S: Iterator<Item = <P as BaseParserMut<S>>::Input>,
// // // {
// // //     type Input = P::Input;
// // //     type Output = P::Output;
// // //     type Error = P::Error;

// // //     fn parse(mut self, stream: S) -> Result<(Self::Output, S), Self::Error> {
// // //         BaseParserMut::parse(&mut self, stream)
// // //     }
// // // }

// // // trait BaseParser<S>
// // // where
// // //     S: Iterator<Item = Self::Input>,
// // // {
// // //     type Input;
// // //     type Output;
// // //     type Error;

// // //     fn parse(&self, stream: S) -> Result<(Self::Output, S), Self::Error>;
// // // }

// // // impl<P, S> BaseParserMut<S> for P
// // // where
// // //     P: BaseParser<S>,
// // //     S: Iterator<Item = <P as BaseParser<S>>::Input>,
// // // {
// // //     type Input = P::Input;
// // //     type Output = P::Output;
// // //     type Error = P::Error;

// // //     fn parse(&mut self, stream: S) -> Result<(Self::Output, S), Self::Error> {
// // //         BaseParser::parse(self, stream)
// // //     }
// // // }

// // impl<F, I, O, E, S> Parser<S> for F
// // where
// //     F: FnOnce(S) -> ParserResult_<I, O, E, S>,
// //     S: Iterator<Item = (usize, I)>,
// // {
// //     type Input = I;
// //     type Output = O;
// //     type Error = E;

// //     fn parse(self, stream: S) -> ParserResult<S, Self> {
// //         self(stream)
// //     }
// // }

// // // trait ParserWithIndex<S>
// // // where
// // //     S: Iterator<Item = (usize, Self::Input)>,
// // // {
// // //     type Input;
// // //     type Output;
// // //     type Error;

// // //     fn parse(self, stream: S) -> Result<(Self::Output, S), (usize, Self::Input, Self::Error)>;
// // // }
