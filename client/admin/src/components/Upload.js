import React from 'react';
import { DropTarget } from 'react-dnd';
import { NativeTypes } from 'react-dnd-html5-backend';

const fileTarget = {
  drop(props, monitor, cmp) {
    cmp.onDropFiles(monitor.getItem().files);
  },
};

const collectDrop = (connect, monitor) => ({
  dropTarget: connect.dropTarget(),
  isOver: monitor.isOver(),
  canDrop: monitor.canDrop(),
  drop: monitor.getDropResult(),
});

class Upload extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      files: [],
    };
  }

  onDropFiles(files) {
    const files$ = files.map((file) => {
      fetch('/api/v1/photo', {
        method: 'POST',
        body: file,
      });

      return Object.assign(file, { loaded: false });
    });

    this.setState({ files: files$ });
  }

  render() {
    const { dropTarget } = this.props;
    const { files } = this.state;

    return dropTarget(
      <div className="upload">
        <ul>
          {files.map(file => <li key={file.name}>{file.name}, {file.loaded ? 'yes' : 'no'}</li>)}
        </ul>
      </div>,
    );
  }
}

export default DropTarget(NativeTypes.FILE, fileTarget, collectDrop)(Upload);
