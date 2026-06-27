import SwiftUI
import UIKit

// MARK: - UIScrollView-based zoomable image

struct ZoomableImageView: UIViewRepresentable {
    let image: UIImage

    func makeUIView(context: Context) -> UIScrollView {
        let scroll = UIScrollView()
        scroll.delegate = context.coordinator
        scroll.minimumZoomScale = 1
        scroll.maximumZoomScale = 6
        scroll.bouncesZoom = true
        scroll.showsHorizontalScrollIndicator = false
        scroll.showsVerticalScrollIndicator = false
        scroll.backgroundColor = .black

        let iv = UIImageView(image: image)
        iv.contentMode = .scaleAspectFit
        iv.isUserInteractionEnabled = true
        scroll.addSubview(iv)
        context.coordinator.imageView = iv

        let doubleTap = UITapGestureRecognizer(
            target: context.coordinator,
            action: #selector(Coordinator.handleDoubleTap(_:))
        )
        doubleTap.numberOfTapsRequired = 2
        scroll.addGestureRecognizer(doubleTap)

        return scroll
    }

    func updateUIView(_ scroll: UIScrollView, context: Context) {
        guard let iv = context.coordinator.imageView else { return }
        iv.image = image
        DispatchQueue.main.async { Self.fit(iv, in: scroll) }
    }

    func makeCoordinator() -> Coordinator { Coordinator() }

    static func fit(_ iv: UIImageView, in scroll: UIScrollView) {
        guard let img = iv.image else { return }
        let size = scroll.bounds.size
        guard size.width > 0, size.height > 0 else { return }
        let scale = min(size.width / img.size.width, size.height / img.size.height)
        iv.frame = CGRect(origin: .zero,
                          size: CGSize(width: img.size.width * scale, height: img.size.height * scale))
        scroll.contentSize = iv.frame.size
        scroll.zoomScale = 1
        Self.center(iv, in: scroll)
    }

    static func center(_ iv: UIImageView, in scroll: UIScrollView) {
        let ox = max((scroll.bounds.width  - iv.frame.width)  / 2, 0)
        let oy = max((scroll.bounds.height - iv.frame.height) / 2, 0)
        iv.center = CGPoint(x: iv.frame.width / 2 + ox, y: iv.frame.height / 2 + oy)
    }

    final class Coordinator: NSObject, UIScrollViewDelegate {
        weak var imageView: UIImageView?

        func viewForZooming(in scrollView: UIScrollView) -> UIView? { imageView }

        func scrollViewDidZoom(_ scrollView: UIScrollView) {
            guard let iv = imageView else { return }
            ZoomableImageView.center(iv, in: scrollView)
        }

        @objc func handleDoubleTap(_ gr: UITapGestureRecognizer) {
            guard let scroll = gr.view as? UIScrollView else { return }
            if scroll.zoomScale > 1 {
                scroll.setZoomScale(1, animated: true)
            } else {
                let pt = gr.location(in: imageView)
                scroll.zoom(to: CGRect(x: pt.x - 50, y: pt.y - 50, width: 100, height: 100),
                            animated: true)
            }
        }
    }
}

// MARK: - Full-screen presenter

struct FullScreenPhotoView: View {
    let assetName: String?
    let url: URL?
    @Environment(\.dismiss) private var dismiss
    @State private var loadedImage: UIImage?

    private var localImage: UIImage? {
        assetName.flatMap { UIImage(named: $0) }
    }

    var body: some View {
        ZStack(alignment: .topTrailing) {
            Color.black.ignoresSafeArea()

            if let img = localImage ?? loadedImage {
                ZoomableImageView(image: img)
                    .ignoresSafeArea()
            } else if url != nil {
                ProgressView().tint(.white)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }

            Button { dismiss() } label: {
                Image(systemName: "xmark.circle.fill")
                    .font(.title)
                    .symbolRenderingMode(.hierarchical)
                    .foregroundStyle(.white)
                    .padding()
            }
        }
        .statusBarHidden()
        .task {
            guard localImage == nil, loadedImage == nil, let url else { return }
            if let (data, _) = try? await URLSession.shared.data(from: url) {
                loadedImage = UIImage(data: data)
            }
        }
    }
}
