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
    @State private var code: String = "let rec print_int x = x in\nlet rec f x = x + 123 in\nlet rec g y = f in\nprint_int ((g 456) 789)"
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
            TextEditor(text: $code)
                .font(ContentView.chatFont)
                .scrollIndicators(.never)
                .scrollContentBackground(.hidden)
                .padding(.vertical, 6)
                .padding(.horizontal, 4)
                .background(Color(.textBackgroundColor))
                .cornerRadius(10)
                .frame(height: 100)
                .overlay(
                    RoundedRectangle(cornerRadius: 10)
                        .stroke(Color.gray.opacity(0.5), lineWidth: 1)
                )
                .autocorrectionDisabled()
                .focusEffectDisabled()
                .focused($isFocused)
                .toolbar {
                    ToolbarItem(placement: .primaryAction) {
                        Button(action: { compiler.compile(code) }) {
                            Label("Compile", systemImage: compiler.isProcessing ? "hammer.fill" : "play.fill")
                        }
                        .keyboardShortcut(.return, modifiers: [.command])
                        .disabled(code.isEmpty || compiler.isProcessing)
                    }
                }
        }
        .padding()
        .onAppear { isFocused = true }
    }
}

#Preview {
    ContentView()
}
