use super::util;

#[test]
fn test_sext() {
    let base = 0b11_0000 as u16;
    let expected = 0b1111_1111_1111_0000 as u16;
    let actual = util::sext(base, 5);
    assert_eq!(expected, actual);
}

#[test]
fn test_imm5() {
    let base = 0b10_0000 as u16;
    let expected = 0;
    let actual = util::imm5(base);
    assert_eq!(expected, actual);
}

#[test]
fn test_offset6() {
    let base = 0b10_0000 as u16;
    let expected = 0b1111_1111_1110_0000 as u16;
    let actual = util::offset6(base);
    assert_eq!(expected, actual);
}

#[test]
fn test_bit_at() {
    let base = 0b10_0000 as u16;
    assert_eq!(util::bit_at(base, 5), true);
    assert_eq!(util::bit_at(base, 4), false);
}
