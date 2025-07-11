import SwiftUI

enum Chat: Hashable, Error {
    case request(text: String)
    case response(tag: String, text: String)
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
