$(document).on('click', '#streamingUpload', () => {

    const fileInput = document.getElementById('upload');
    file = fileInput.files[0]

    if (file) {

        console.log(`open file ${file.size}`);

        const port = window.location.port ? window.location.port : (window.location.protocol === 'http:' ? 80 : 443);
        const host = window.location.hostname
        const ws = new WebSocket(`ws://${host}:${port}/beta/users/upload`);
        ws.onopen = () => {

            console.log('ws open, starting send');

            const CHUNK_SIZE = 50;
            ws.send(file.size)

            for (let i = 0; i < file.size; i += CHUNK_SIZE) {
                ws.send(file.slice(i, i + CHUNK_SIZE));
            }
            ws.send("COMPLETE")

            console.log('complete')
        }

        ws.bufferedAmount()/
        // ws.close();

        console.log('ws closed');
    }
});
