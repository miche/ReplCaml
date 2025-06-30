import SwiftUI

struct ContentView: View {
    @State private var e = MinCaml()
    @State private var messages: [Message] = []
    @State private var inputText: String = ""
    @FocusState private var isFocused: Bool
    @State private var msgNo: Int = 0
    static private let chatFont = Font.custom("SF Mono", size: 14)

    struct Message: Hashable {
        let no: Int
        let text: String
        let isUserMessage: Bool
    }

    struct ChatView: View {
        let text: String
        let color: Color
        var body: some View {
            Text(text)
                .font(chatFont)
                .padding(.vertical, 4)
                .padding(.horizontal, 10)
                .background(color.opacity(0.2))
                .cornerRadius(8)
        }
    }
    struct RequestView: View {
        var text: String = ""
        var body: some View {
            HStack(alignment: .top, spacing: 8) {
                Spacer()
                ChatView(text: text, color: .blue)
            }
        }
    }
    struct ResultView: View {
        var text: String = ""
        var body: some View {
            HStack(alignment: .top, spacing: 8) {
                ChatView(text: text, color: .green)
                Spacer()
            }
        }
    }

    var body: some View {
        VStack {
            ScrollViewReader { reader in
                ScrollView {
                    ForEach(messages, id: \.self) { message in
                        if message.isUserMessage {
                            RequestView(text: message.text)
                        } else {
                            ResultView(text: message.text)
                        }
                    }
                }
                .padding()
                .onChange(of: messages) {
                    if let lastMessage = messages.last {
                        reader.scrollTo(lastMessage, anchor: .bottom)
                    }
                }
            }
            TextField("def/extern", text: $inputText, axis: .vertical)
                .font(ContentView.chatFont)
                .textFieldStyle(.roundedBorder)
                .autocorrectionDisabled()
                .focusEffectDisabled()
                .focused($isFocused)
                .onSubmit(sendMessage)
                .padding()
        }
        .onAppear { isFocused = true }
    }

    func sendMessage() {
        if !inputText.isEmpty {
            messages.append(Message(no: msgNo, text: inputText, isUserMessage: true))
            msgNo += 1
            if let response = e.handle(inputText) {
                response.forEach { res in
                    messages.append(Message(no: msgNo, text: res, isUserMessage: false))
                    msgNo += 1
                }
            }
        }
    }
}
#Preview {
    ContentView()
}

