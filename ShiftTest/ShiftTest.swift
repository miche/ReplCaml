import Testing
@testable import Shift

struct ShiftTest {

    @Test func example() async throws {
        // Write your test here and use APIs like `#expect(...)` to check expected conditions.
        #expect(Parser().parse("1") == PEGResult<Syntax>(true, [.INT(1)], ""))
    }

}
