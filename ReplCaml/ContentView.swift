import SwiftUI

struct ChatMsg: Identifiable, Hashable {
    var id: UUID
    var chat: Chat
}

enum Chat: Hashable {
    case request(text: String)
    case response(tag: String, text: String)
    case info(tag: String, text: String)
    case error(tag: String, text: String)

    static func == (lhs: Chat, rhs: Chat) -> Bool {
        switch (lhs, rhs) {
        case (.request(let l), .request(let r)): return l == r
        case (.response(_, let l), .response(_, let r)): return l == r // ignoring tag
        default: return false
        }
    }
}

struct ContentView: View {
    @State private var code: String = ""
    @FocusState private var isFocused: Bool
    static private let chatFont = Font.custom("SF Mono", size: 14)
    @StateObject private var compiler = Compiler()

    struct Bubble: View {
        var text: String
        var width: CGFloat?
        var align: Alignment
        var color: Color
        var body: some View {
            Text(text)
                .font(chatFont)
                .textSelection(.enabled)
                .padding(.horizontal, 12)
                .padding(.vertical, 4)
                .background(color.opacity(0.2))
                .cornerRadius(8)
                .frame(maxWidth: width, alignment: align)
        }
    }

    var body: some View {
        VStack {
            ZStack {
                ScrollViewReader { reader in
                    ScrollView(showsIndicators: false) {
                        VStack(alignment: .leading, spacing: 4) {
                            ForEach(compiler.out, id: \.self) { response in
                                HStack(alignment: .bottom) {
                                    switch response.chat {
                                    case .request(text: let text):
                                        Bubble(text: text, width: .infinity, align: .trailing, color: .accentColor)
                                    case .response(tag: let tag, text: let text):
                                        Bubble(text: text, align: .leading, color: .gray)
                                        Text(tag).font(.footnote)
                                    case .info(tag: let tag, text: let text):
                                        Bubble(text: text, align: .leading, color: .cyan)
                                        Text(tag).font(.footnote)
                                    case .error(tag: let tag, text: let text):
                                        Bubble(text: text, align: .leading, color: .red)
                                        Text(tag).font(.footnote).foregroundStyle(.red)
                                    }
                                }
                            }
                        }
                    }.onChange(of: compiler.out) {
                        if let lastResponse = compiler.out.last {
                            reader.scrollTo(lastResponse, anchor: .bottom)
                        }
                    }
                }
                if compiler.isProcessing { ProgressView().background(.clear) }
            }
            TextField("let rec ... ", text: $code, axis: .vertical)
                .font(ContentView.chatFont)
                .textFieldStyle(.roundedBorder)
                .autocorrectionDisabled()
                .focusEffectDisabled()
                .focused($isFocused)
                .onSubmit { compiler.compile(code) }
        }
        .padding()
        .onAppear { isFocused = true }
    }
}

#Preview {
    ContentView()
}
