import SwiftUI

struct ContentView: View {
    @State private var inputText: String = ""
    @State private var messages: [Chat] = []
    @State private var isProcessing = false
    @FocusState private var isFocused: Bool
    static private let chatFont = Font.custom("SF Mono", size: 14)

    enum Chat: Hashable, Error {
        case request(text: String)
        case response(tag: String, text: String)
    }

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
                            ForEach(messages, id: \.self) { response in
                                HStack(alignment: .bottom) {
                                    switch response {
                                    case .request(text: let text):
                                        Bubble(text: text, width: .infinity, align: .trailing, color: .accentColor)
                                    case .response(tag: let tag, text: let text):
                                        Bubble(text: text, align: .leading, color: .gray)
                                        Text(tag).font(.footnote)
                                    }
                                }
                            }
                        }
                    }.onChange(of: messages) {
                        if let lastResponse = messages.last {
                            reader.scrollTo(lastResponse, anchor: .bottom)
                        }
                    }
                }
                if isProcessing { ProgressView().background(.clear) }
            }
            TextField("let rec ... ", text: $inputText, axis: .vertical)
                .font(ContentView.chatFont)
                .textFieldStyle(.roundedBorder)
                .autocorrectionDisabled()
                .focusEffectDisabled()
                .focused($isFocused)
                .onSubmit { Task { await performAsyncTask() } }
        }
        .padding()
        .onAppear { isFocused = true }
    }

    @MainActor
    func performAsyncTask() async {
        guard !isProcessing else { return }
        isProcessing = true
        defer { isProcessing = false }
        do {
            messages.append(.request(text: inputText))
            let result = try await compile(inputText)
            messages.append(contentsOf: result)
        } catch {
            messages.append(error as! ContentView.Chat)
        }
    }

    func compile(_ code: String) async throws -> [Chat] {
        if let response = await MinCaml.f(code) {
            return response.reduce([]) { acc, tt in
                if acc.isEmpty { return [.response(tag: tt.1, text: tt.0)] }
                else {
                    if case .response(_, let text) = acc.last!, text != tt.0 {
                        return acc + [.response(tag: tt.1, text: tt.0)]
                    } else { return acc }
                }
            }
        } else { throw Chat.response(tag: "compile failed", text: code) }
    }
}

#Preview {
    ContentView()
}
